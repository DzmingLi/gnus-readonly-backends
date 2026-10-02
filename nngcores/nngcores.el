;;; nngcores.el --- GCORES user talks in Gnus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: news

;;; Commentary:

;; Read-only Gnus backend for a GCORES user's talks.  Each user is a group,
;; with stable article numbers for Gnus read and tick marks.  No login is
;; needed.  Current Draft.js blocks are rendered into HTML, including lead
;; images and gallery images.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'xml)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-start)
(require 'nnoo)
(require 'nnheader)
(require 'rfc2047)
(require 'mail-parse)

(defgroup nngcores nil "GCORES in Gnus." :group 'gnus)
(defcustom nngcores-request-timeout 30
  "Seconds to wait for the public GCORES API."
  :type 'number :group 'nngcores)

(nnoo-declare nngcores)
(defvoo nngcores-directory (expand-file-name "nngcores/" gnus-directory)
  "Directory for local talk snapshots and stable article numbers.")
(defvoo nngcores--state nil)
(defvoo nngcores-status-string "")
(nnoo-define-basics nngcores)
(cl-defstruct nngcores--db file groups)
(defvar nngcores--databases (make-hash-table :test #'equal))

(defun nngcores--load (file)
  "Read FILE as JSON without evaluating code."
  (let ((db (make-nngcores--db :file file)))
    (when (file-readable-p file)
      (let ((data (with-temp-buffer
                    (insert-file-contents file)
                    (json-parse-buffer :object-type 'plist :array-type 'list
                                       :null-object nil :false-object nil))))
        (unless (equal (plist-get data :version) 1)
          (error "Unsupported GCORES snapshot"))
        (setf (nngcores--db-groups db) (plist-get data :groups))))
    db))

(defun nngcores--save (db)
  "Atomically save DB without Gnus reading marks."
  (let* ((file (nngcores--db-file db))
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
                        (nngcores--db-groups db))))))))
          (rename-file temp file t))
      (when (and temp (file-exists-p temp)) (delete-file temp)))))

(deffoo nngcores-open-server (server &optional defs _connectionless)
  (condition-case problem
      (progn
        (nnoo-change-server 'nngcores server defs)
        (let ((file (expand-file-name
                     (concat (secure-hash 'sha256 server) ".json")
                     nngcores-directory)))
          (setq nngcores--state
                (or (gethash file nngcores--databases)
                    (puthash file (nngcores--load file) nngcores--databases))))
        t)
    (error (nnheader-report 'nngcores "%s" (error-message-string problem)))))

(defun nngcores--select (&optional server)
  "Return SERVER's local database."
  (when server
    (unless (nngcores-open-server server) (error "%s" nngcores-status-string)))
  (or nngcores--state (error "No GCORES server selected")))

(defun nngcores--user-id (group)
  "Extract numeric user ID from GROUP."
  (unless (and (stringp group)
               (string-match "\\`talks\\.\\([1-9][0-9]*\\)\\'" group))
    (error "Expected talks.USER-ID"))
  (match-string 1 group))

(defun nngcores--group (db name &optional create)
  "Find NAME in DB, optionally CREATE it."
  (or (cl-find name (nngcores--db-groups db)
               :key (lambda (item) (plist-get item :name)) :test #'equal)
      (when create
        (let ((record (list :name name :user-id (nngcores--user-id name)
                            :high 0 :entries nil)))
          (push record (nngcores--db-groups db))
          record))))

(defun nngcores--absolute-image (path)
  "Return an absolute GCORES image URL for PATH."
  (when (and (stringp path) (not (string-empty-p path)))
    (if (string-match-p "\\`https?://" path)
        path
      (concat "https://image.gcores.com/" (string-remove-prefix "/" path)))))

(defun nngcores--draft-entity (range entity-map)
  "Render a Draft.js entity RANGE found in ENTITY-MAP."
  (let* ((key (plist-get range :key))
         (entity (plist-get entity-map (intern (format ":%s" key))))
         (data (plist-get entity :data)))
    (pcase (plist-get entity :type)
      ("GALLERY"
       (concat
        (mapconcat
         (lambda (image)
           (if-let* ((url (nngcores--absolute-image (plist-get image :path))))
               (format "<figure><img src=\"%s\"></figure>"
                       (xml-escape-string url))
             ""))
         (plist-get data :images) "")
        (when-let* ((caption (plist-get data :caption)))
          (unless (string-empty-p caption)
            (format "<p>%s</p>" (xml-escape-string caption))))))
      (_ ""))))

(defun nngcores--draft-html (content)
  "Render Draft.js CONTENT blocks and gallery entities as HTML."
  (let ((entities (plist-get content :entityMap)))
    (mapconcat
     (lambda (block)
       (let ((type (plist-get block :type))
             (text (or (plist-get block :text) "")))
         (if (equal type "atomic")
             (mapconcat (lambda (range) (nngcores--draft-entity range entities))
                        (plist-get block :entityRanges) "")
           (if (string-empty-p text) ""
             (format "<p>%s</p>" (xml-escape-string text))))))
     (plist-get content :blocks) "")))

(defun nngcores--parse-content (content)
  "Parse serialized GCORES Draft.js CONTENT into HTML."
  (if (not (and (stringp content) (not (string-empty-p content))))
      ""
    (let ((parsed (json-parse-string
                   content :object-type 'plist :array-type 'list
                   :null-object nil :false-object nil)))
      (unless (plist-member parsed :blocks)
        (error "GCORES content has no Draft.js blocks"))
      (nngcores--draft-html parsed))))

(defun nngcores--included-author (item included)
  "Find ITEM's author resource in INCLUDED."
  (let* ((relationship (plist-get
                        (plist-get (plist-get item :relationships) :user) :data))
         (id (plist-get relationship :id))
         (type (plist-get relationship :type)))
    (seq-find (lambda (candidate)
                (and (equal id (plist-get candidate :id))
                     (equal type (plist-get candidate :type))))
              included)))

(defun nngcores--normalize (item included)
  "Convert API ITEM with INCLUDED resources into one Gnus article."
  (let* ((id (format "%s" (plist-get item :id)))
         (type (plist-get item :type))
         (attributes (plist-get item :attributes))
         (title (or (plist-get attributes :title) "机核动态"))
         (cover (or (plist-get attributes :cover)
                    (plist-get attributes :thumb)))
         (author-object (nngcores--included-author item included))
         (author-attributes (plist-get author-object :attributes))
         (author (or (plist-get author-attributes :nickname)
                     (plist-get author-object :nickname) "机核用户"))
         (intro (or (plist-get attributes :desc)
                    (plist-get attributes :excerpt)))
         (body (nngcores--parse-content (plist-get attributes :content))))
    (unless (and (stringp type) (string-match-p "\\`[[:alnum:]-]+\\'" type)
                 (string-match-p "\\`[0-9]+\\'" id))
      (error "GCORES item has an invalid type or ID"))
    (list :guid (format "gcores-%s-%s" type id)
          :title title
          :link (format "https://www.gcores.com/%s/%s" type id)
          :date (or (plist-get attributes :created-at)
                    (plist-get attributes :published-at))
          :author author
          :body (concat
                 (when-let* ((image (nngcores--absolute-image cover)))
                   (format "<p><img src=\"%s\" alt=\"%s\"></p>"
                           (xml-escape-string image) (xml-escape-string title)))
                 (when intro
                   (format "<p>%s</p>" (xml-escape-string intro)))
                 body))))

(defun nngcores--api-url (user-id)
  "Return the public talks URL for USER-ID."
  (concat "https://www.gcores.com/gapi/v1/users/" user-id "/talks?"
          (url-build-query-string
           '(("page[limit]" "60") ("sort" "-created-at")
             ("include" "user")))))

(defun nngcores--request-json (url)
  "GET URL and parse its JSON body."
  (with-current-buffer (or (url-retrieve-synchronously
                            url t t nngcores-request-timeout)
                           (error "Could not fetch %s" url))
    (unwind-protect
        (progn
          (goto-char (point-min))
          (unless (looking-at "HTTP/[0-9.]+ 2[0-9][0-9]")
            (error "GCORES API request failed: %s"
                   (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
          (unless (re-search-forward "\r?\n\r?\n" nil t)
            (error "GCORES API response has no body"))
          (json-parse-buffer :object-type 'plist :array-type 'list
                             :null-object nil :false-object nil))
      (kill-buffer (current-buffer)))))

(defun nngcores--fetch (record)
  "Fetch the current talk items for RECORD."
  (let* ((payload (nngcores--request-json
                   (nngcores--api-url (plist-get record :user-id))))
         (data (plist-get payload :data))
         (included (plist-get payload :included)))
    (unless (listp data) (error "GCORES API returned no item list"))
    (mapcar
     (lambda (item) (nngcores--normalize item included))
     (seq-take
      (seq-filter (lambda (item)
                    (not (member (plist-get item :type) '("radios" "videos"))))
                  data)
      30))))

(defun nngcores--refresh (db record)
  "Merge current talks into RECORD, preserving stable article numbers."
  (let ((entries (plist-get record :entries))
        (high (plist-get record :high)))
    (dolist (item (nngcores--fetch record))
      (let ((old (cl-find (plist-get item :guid) entries
                          :key (lambda (entry) (plist-get entry :guid))
                          :test #'equal)))
        (if old
            (progn
              (setq item (plist-put item :number (plist-get old :number)))
              (setq entries (cons item (delq old entries))))
          (setq high (1+ high)
                item (plist-put item :number high))
          (push item entries))))
    (setf (plist-get record :entries) entries
          (plist-get record :high) high)
    (nngcores--save db)))

(defun nngcores--message-id (entry)
  "Stable Message-ID for ENTRY."
  (format "<%s@gcores.invalid>" (plist-get entry :guid)))

(defun nngcores--entry (record article)
  "Find ARTICLE by number or Message-ID in RECORD."
  (cl-find-if
   (lambda (entry)
     (if (integerp article)
         (= article (plist-get entry :number))
       (equal article (nngcores--message-id entry))))
   (plist-get record :entries)))

(defun nngcores--header (entry)
  "Construct a Gnus mail header for ENTRY."
  (make-full-mail-header
   (plist-get entry :number)
   (replace-regexp-in-string "[\r\n]+" " " (plist-get entry :title))
   (replace-regexp-in-string "[\r\n]+" " " (plist-get entry :author))
   (condition-case nil
       (let ((system-time-locale "C"))
         (format-time-string "%a, %d %b %Y %T %z"
                             (date-to-time (plist-get entry :date)) t))
     (error "Thu, 01 Jan 1970 00:00:00 +0000"))
   (nngcores--message-id entry) "" 0 0 "" nil))

(deffoo nngcores-request-create-group (group &optional server _args)
  (let ((db (nngcores--select server)))
    (nngcores--group db group t)
    (nngcores--save db)
    t))
(deffoo nngcores-close-group (_group &optional _server) t)
(deffoo nngcores-asynchronous-p () nil)
(deffoo nngcores-request-post (&optional _server)
  (nnheader-report 'nngcores "GCORES talks are read-only here"))

(deffoo nngcores-request-scan (&optional group server)
  (condition-case problem
      (let ((db (nngcores--select server)))
        (dolist (record (if group (list (nngcores--group db group))
                          (nngcores--db-groups db)))
          (when record (nngcores--refresh db record)))
        t)
    (error (nnheader-report 'nngcores "%s" (error-message-string problem)))))

(deffoo nngcores-request-group (group &optional server _fast _info)
  (if-let* ((record (nngcores--group (nngcores--select server) group)))
      (nnheader-insert "211 %d 1 %d %s\n" (length (plist-get record :entries))
                       (plist-get record :high) group t)
    (nnheader-report 'nngcores "Unknown GCORES subscription")))

(deffoo nngcores-request-list (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (record (nngcores--db-groups (nngcores--select server)))
      (insert (format "%s %d 1 n\n" (plist-get record :name)
                      (plist-get record :high)))))
  t)
(deffoo nngcores-retrieve-groups (_groups &optional server)
  (nngcores-request-list server) 'active)
(deffoo nngcores-request-list-newsgroups (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (record (nngcores--db-groups (nngcores--select server)))
      (insert (plist-get record :name) "\t机核动态 · "
              (plist-get record :user-id) "\n")))
  t)

(deffoo nngcores-retrieve-headers (articles &optional group server _fetch-old)
  (let ((record (nngcores--group (nngcores--select server) group)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (number articles)
        (when-let* ((entry (and record (nngcores--entry record number))))
          (nnheader-insert-nov (nngcores--header entry))))))
  'nov)

(deffoo nngcores-request-article (article &optional group server buffer)
  (let* ((record (nngcores--group (nngcores--select server) group))
         (entry (and record (nngcores--entry record article))))
    (if (not entry)
        (nnheader-report 'nngcores "Article is absent from the snapshot")
      (let ((header (nngcores--header entry)))
        (with-current-buffer (or buffer nntp-server-buffer)
          (erase-buffer)
          (insert "From: " (rfc2047-encode-string (mail-header-from header)) "\n"
                  "Subject: " (rfc2047-encode-string (mail-header-subject header)) "\n"
                  "Date: " (mail-header-date header) "\n"
                  "Message-ID: " (mail-header-id header) "\n"
                  "Newsgroups: " group "\n"
                  "Archived-at: <" (plist-get entry :link) ">\n"
                  "MIME-Version: 1.0\nContent-Type: text/html; charset=utf-8\n"
                  "Content-Transfer-Encoding: base64\n\n"
                  (base64-encode-string
                   (encode-coding-string (plist-get entry :body) 'utf-8)) "\n")))
      (cons group (plist-get entry :number)))))

;;;###autoload
(defun nngcores-subscribe-user (user-id)
  "Subscribe to GCORES numeric USER-ID's talks in Gnus."
  (interactive "sGCORES user ID: ")
  (unless (string-match-p "\\`[1-9][0-9]*\\'" user-id)
    (user-error "Enter a numeric GCORES user ID"))
  (unless (gnus-alive-p) (gnus-no-server))
  (let* ((server "gcores.com")
         (method `(nngcores ,server))
         (group (concat "talks." user-id))
         (full (gnus-group-prefixed-name group method)))
    (unless (nngcores-open-server server) (error "%s" nngcores-status-string))
    (with-current-buffer gnus-group-buffer
      (unless (gnus-group-entry full) (gnus-group-make-group group method))
      (gnus-group-change-level full gnus-level-default-subscribed))
    (unless (nngcores-request-scan group server)
      (error "%s" nngcores-status-string))
    (with-current-buffer gnus-group-buffer
      (gnus-group-read-group t t full))))

(provide 'nngcores)
;;; nngcores.el ends here
