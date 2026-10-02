;;; nnpublicinbox.el --- Public-inbox threads in Gnus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: news mail

;;; Commentary:
;; A read-only Gnus backend for individual public-inbox mail threads.  Each
;; message remains an article with its original Message-ID and References.
;; Read marks belong to Gnus; the local snapshot only maps IDs to stable
;; article numbers and caches the original RFC 822 messages.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-start)
(require 'nnoo)
(require 'nnheader)
(require 'mail-parse)
(require 'rfc2047)

(defgroup nnpublicinbox nil "Public-inbox threads in Gnus." :group 'gnus)
(defcustom nnpublicinbox-request-timeout 45
  "Seconds to wait for a public-inbox archive."
  :type 'number :group 'nnpublicinbox)

(nnoo-declare nnpublicinbox)
(defvoo nnpublicinbox-directory
  (expand-file-name "nnpublicinbox/" gnus-directory)
  "Directory containing public-inbox thread snapshots.")
(defvoo nnpublicinbox--state nil)
(defvoo nnpublicinbox-status-string "")
(nnoo-define-basics nnpublicinbox)
(cl-defstruct nnpublicinbox--db file groups)
(defvar nnpublicinbox--databases (make-hash-table :test #'equal))

(defun nnpublicinbox--load (file)
  "Load snapshot FILE as data."
  (let ((db (make-nnpublicinbox--db :file file)))
    (when (file-readable-p file)
      (let ((data (with-temp-buffer
                    (insert-file-contents file)
                    (json-parse-buffer :object-type 'plist :array-type 'list
                                       :null-object nil :false-object nil))))
        (unless (= (plist-get data :version) 1)
          (error "Unsupported public-inbox snapshot"))
        (setf (nnpublicinbox--db-groups db) (plist-get data :groups))))
    db))

(defun nnpublicinbox--save (db)
  "Atomically save DB."
  (let* ((file (nnpublicinbox--db-file db))
         (directory (file-name-directory file)) temp)
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temp (make-temp-file (expand-file-name ".snapshot-" directory)))
          (set-file-modes temp #o600)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file temp
              (insert
               (json-serialize
                (list :version 1
                      :groups
                      (vconcat
                       (mapcar
                        (lambda (group)
                          (let ((copy (copy-sequence group)))
                            (setf (plist-get copy :entries)
                                  (vconcat (plist-get copy :entries)))
                            copy))
                        (nnpublicinbox--db-groups db))))))))
          (rename-file temp file t))
      (when (and temp (file-exists-p temp)) (delete-file temp)))))

(deffoo nnpublicinbox-open-server (server &optional defs _connectionless)
  (condition-case problem
      (progn
        (nnoo-change-server 'nnpublicinbox server defs)
        (let ((file (expand-file-name
                     (concat (secure-hash 'sha256 server) ".json")
                     nnpublicinbox-directory)))
          (setq nnpublicinbox--state
                (or (gethash file nnpublicinbox--databases)
                    (puthash file (nnpublicinbox--load file)
                             nnpublicinbox--databases))))
        t)
    (error (nnheader-report 'nnpublicinbox "%s"
                            (error-message-string problem)))))

(defun nnpublicinbox--select (&optional server)
  "Return SERVER's snapshot."
  (when server
    (unless (nnpublicinbox-open-server server)
      (error "%s" nnpublicinbox-status-string)))
  (or nnpublicinbox--state (error "No public-inbox server selected")))

(defun nnpublicinbox--group (db name &optional create)
  "Find NAME in DB, optionally CREATE it."
  (or (cl-find name (nnpublicinbox--db-groups db)
               :key (lambda (item) (plist-get item :name)) :test #'equal)
      (when create
        (let ((record (list :name name :high 0 :entries nil)))
          (push record (nnpublicinbox--db-groups db))
          record))))

(defun nnpublicinbox--url (server group)
  "Build thread mbox URL from SERVER and GROUP."
  (unless (and (string-match-p "\\`https://[[:alnum:].-]+\\'" server)
               (string-match
                "\\`\\([[:alnum:]_-]+\\)/\\([^/[:space:]]+\\)\\'"
                group))
    (error "Expected list/MESSAGE-ID on an HTTPS public-inbox server"))
  (format "%s/%s/%s/t.mbox.gz" server (match-string 1 group)
          (match-string 2 group)))

(defun nnpublicinbox--fetch (server group)
  "Fetch and decompress SERVER's GROUP mbox."
  (let ((url (nnpublicinbox--url server group)))
    (with-current-buffer
        (or (url-retrieve-synchronously url t t nnpublicinbox-request-timeout)
            (error "Could not fetch %s" url))
      (unwind-protect
          (progn
            (goto-char (point-min))
            (unless (looking-at "HTTP/[0-9.]+ 2[0-9][0-9]")
              (error "Public-inbox request failed: %s"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
            (unless (re-search-forward "\r?\n\r?\n" nil t)
              (error "Public-inbox response has no body"))
            (let ((bytes (encode-coding-string
                          (buffer-substring-no-properties (point) (point-max))
                          'binary)))
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert bytes)
                (unless (zlib-decompress-region (point-min) (point-max))
                  (error "Invalid public-inbox gzip response"))
                (decode-coding-string (buffer-string) 'utf-8))))
        (kill-buffer (current-buffer))))))

(defun nnpublicinbox--message (raw)
  "Parse one mboxrd message RAW into an entry."
  (with-temp-buffer
    (insert raw)
    (goto-char (point-min))
    (unless (re-search-forward "^\r?$" nil t)
      (error "Public-inbox message has no header/body separator"))
    (let* ((body-start (point))
           (fields (save-restriction
                     (narrow-to-region (point-min) body-start)
                     (mapcar (lambda (name) (mail-fetch-field name))
                             '("Message-ID" "Subject" "From" "Date"
                               "References" "In-Reply-To"))))
           (id (string-trim (or (nth 0 fields) "")))
           (subject (rfc2047-decode-string (or (nth 1 fields) "")))
           (from (rfc2047-decode-string (or (nth 2 fields) "")))
           (date (or (nth 3 fields) ""))
           (refs (or (nth 4 fields) (nth 5 fields) "")))
      (unless (string-match-p "\\`<[^<>[:space:]]+>\\'" id)
        (error "Public-inbox message has no valid Message-ID"))
      ;; mboxrd quotes body lines beginning with From; undo one quoting level.
      (goto-char body-start)
      (while (re-search-forward "^>\\(>*From \\)" nil t)
        (replace-match "\\1"))
      (list :id id :subject subject :from from :date date
            :references (replace-regexp-in-string "[\r\n[:space:]]+" " " refs)
            :raw (buffer-string)))))

(defun nnpublicinbox--parse-mbox (mbox)
  "Parse MBOX into unique original mail messages."
  (with-temp-buffer
    (insert mbox)
    (goto-char (point-min))
    (let (starts entries)
      (while (re-search-forward "^From mboxrd@z .*\n" nil t)
        (push (cons (match-beginning 0) (point)) starts))
      (setq starts (nreverse starts))
      (unless starts (error "Public-inbox response is not mboxrd"))
      (cl-loop for (_ . body-start) in starts
               for next in (append (mapcar #'car (cdr starts))
                                   (list (point-max)))
               do (push (nnpublicinbox--message
                         (buffer-substring-no-properties body-start next))
                        entries))
      (nreverse entries))))

(defun nnpublicinbox--refresh (db record server)
  "Merge SERVER's thread into RECORD, keeping stable article numbers."
  (let ((entries (plist-get record :entries))
        (high (plist-get record :high)))
    (dolist (message (nnpublicinbox--parse-mbox
                      (nnpublicinbox--fetch server (plist-get record :name))))
      (let ((old (cl-find (plist-get message :id) entries
                          :key (lambda (entry) (plist-get entry :id))
                          :test #'equal)))
        (if old
            (setq message (plist-put message :number (plist-get old :number))
                  entries (cons message (delq old entries)))
          (setq high (1+ high)
                message (plist-put message :number high))
          (push message entries))))
    (setf (plist-get record :entries) entries
          (plist-get record :high) high)
    (nnpublicinbox--save db)))

(defun nnpublicinbox--entry (record article)
  "Find ARTICLE by number or Message-ID in RECORD."
  (cl-find-if (lambda (entry)
                (if (integerp article)
                    (= article (plist-get entry :number))
                  (equal article (plist-get entry :id))))
              (plist-get record :entries)))

(defun nnpublicinbox--header (entry)
  "Construct a native Gnus mail header for ENTRY."
  (make-full-mail-header
   (plist-get entry :number)
   (replace-regexp-in-string "[\r\n]+" " " (plist-get entry :subject))
   (replace-regexp-in-string "[\r\n]+" " " (plist-get entry :from))
   (plist-get entry :date)
   (plist-get entry :id)
   (plist-get entry :references)
   0 0 "" nil))

(deffoo nnpublicinbox-request-create-group (group &optional server _args)
  (condition-case problem
      (let ((db (nnpublicinbox--select server)))
        (nnpublicinbox--url server group)
        (nnpublicinbox--group db group t)
        (nnpublicinbox--save db)
        t)
    (error (nnheader-report 'nnpublicinbox "%s"
                            (error-message-string problem)))))
(deffoo nnpublicinbox-close-group (_group &optional _server) t)
(deffoo nnpublicinbox-asynchronous-p () nil)
(deffoo nnpublicinbox-request-post (&optional _server)
  (nnheader-report 'nnpublicinbox "Public-inbox archive is read-only"))

(deffoo nnpublicinbox-request-scan (&optional group server)
  (condition-case problem
      (let ((db (nnpublicinbox--select server)))
        (dolist (record (if group (list (nnpublicinbox--group db group))
                          (nnpublicinbox--db-groups db)))
          (when record (nnpublicinbox--refresh db record server)))
        t)
    (error (nnheader-report 'nnpublicinbox "%s"
                            (error-message-string problem)))))

(deffoo nnpublicinbox-request-group (group &optional server _fast _info)
  (if-let* ((record (nnpublicinbox--group
                     (nnpublicinbox--select server) group)))
      (nnheader-insert "211 %d 1 %d %s\n"
                       (length (plist-get record :entries))
                       (plist-get record :high) group t)
    (nnheader-report 'nnpublicinbox "Unknown thread subscription")))

(deffoo nnpublicinbox-request-list (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (record (nnpublicinbox--db-groups (nnpublicinbox--select server)))
      (insert (format "%s %d 1 n\n" (plist-get record :name)
                      (plist-get record :high)))))
  t)
(deffoo nnpublicinbox-retrieve-groups (_groups &optional server)
  (nnpublicinbox-request-list server) 'active)
(deffoo nnpublicinbox-request-list-newsgroups (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (record (nnpublicinbox--db-groups (nnpublicinbox--select server)))
      (insert (plist-get record :name) "\tPublic-inbox thread\n")))
  t)
(deffoo nnpublicinbox-retrieve-headers (articles &optional group server _fetch-old)
  (let ((record (nnpublicinbox--group (nnpublicinbox--select server) group)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (number articles)
        (when-let* ((entry (and record (nnpublicinbox--entry record number))))
          (nnheader-insert-nov (nnpublicinbox--header entry))))))
  'nov)
(deffoo nnpublicinbox-request-article (article &optional group server buffer)
  (let* ((record (nnpublicinbox--group (nnpublicinbox--select server) group))
         (entry (and record (nnpublicinbox--entry record article))))
    (if (not entry)
        (nnheader-report 'nnpublicinbox "Message is absent from snapshot")
      (with-current-buffer (or buffer nntp-server-buffer)
        (erase-buffer)
        (insert (plist-get entry :raw)))
      (cons group (plist-get entry :number)))))

;;;###autoload
(defun nnpublicinbox-subscribe-thread (url)
  "Subscribe to a public-inbox thread URL in Gnus."
  (interactive "sPublic-inbox thread URL: ")
  (unless (string-match
           "\\`\\(https://[[:alnum:].-]+\\)/\\([[:alnum:]_-]+/[^/[:space:]]+\\)/?\\'"
           url)
    (user-error "Expected https://HOST/LIST/MESSAGE-ID"))
  (let ((server (match-string 1 url))
        (group (match-string 2 url)))
    (unless (gnus-alive-p) (gnus-no-server))
    (let* ((method `(nnpublicinbox ,server))
           (full (gnus-group-prefixed-name group method)))
      (unless (nnpublicinbox-open-server server)
        (error "%s" nnpublicinbox-status-string))
      (with-current-buffer gnus-group-buffer
        (unless (gnus-group-entry full)
          (gnus-group-make-group group method))
        (gnus-group-change-level full gnus-level-default-subscribed))
      (unless (nnpublicinbox-request-scan group server)
        (error "%s" nnpublicinbox-status-string))
      (with-current-buffer gnus-group-buffer
        (gnus-group-read-group t t full)))))

(provide 'nnpublicinbox)
;;; nnpublicinbox.el ends here
