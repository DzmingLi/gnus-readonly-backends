;;; nnjanestreet.el --- Full Jane Street blog posts in Gnus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: news

;;; Commentary:

;; RSS supplies the index; article pages supply full HTML and author names.
;; Each server has one read-only posts group.  Gnus owns reading marks.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'xml)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-start)
(require 'nnoo)
(require 'nnheader)
(require 'rfc2047)
(require 'mail-parse)

(defgroup nnjanestreet nil "Jane Street in Gnus." :group 'gnus)
(defcustom nnjanestreet-request-timeout 30
  "Seconds to wait for a public Jane Street feed or post page."
  :type 'number :group 'nnjanestreet)
(nnoo-declare nnjanestreet)
(defvoo nnjanestreet-directory (expand-file-name "nnjanestreet/" gnus-directory)
  "Directory for local post snapshots and stable article numbers.")
(defvoo nnjanestreet--state nil)
(defvoo nnjanestreet-status-string "")
(nnoo-define-basics nnjanestreet)
(cl-defstruct nnjanestreet--db file groups)
(defvar nnjanestreet--databases (make-hash-table :test #'equal))

(defun nnjanestreet--load (file)
  "Read FILE as JSON without evaluating code."
  (let ((db (make-nnjanestreet--db :file file)))
    (when (file-readable-p file)
      (let ((data (with-temp-buffer
                    (insert-file-contents file)
                    (json-parse-buffer :object-type 'plist :array-type 'list
                                       :null-object nil :false-object nil))))
        (unless (equal (plist-get data :version) 1)
          (error "Unsupported Jane Street snapshot"))
        (setf (nnjanestreet--db-groups db) (plist-get data :groups))))
    db))

(defun nnjanestreet--save (db)
  "Atomically save DB without Gnus reading marks."
  (let* ((file (nnjanestreet--db-file db))
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
                        (nnjanestreet--db-groups db))))))))
          (rename-file temp file t))
      (when (and temp (file-exists-p temp)) (delete-file temp)))))

(deffoo nnjanestreet-open-server (server &optional defs _connectionless)
  (condition-case problem
      (progn
        (nnoo-change-server 'nnjanestreet server defs)
        (let ((file (expand-file-name
                     (concat (secure-hash 'sha256 server) ".json")
                     nnjanestreet-directory)))
          (setq nnjanestreet--state
                (or (gethash file nnjanestreet--databases)
                    (puthash file (nnjanestreet--load file) nnjanestreet--databases))))
        t)
    (error (nnheader-report 'nnjanestreet "%s" (error-message-string problem)))))

(defun nnjanestreet--select (&optional server)
  "Return SERVER's local database."
  (when server
    (unless (nnjanestreet-open-server server) (error "%s" nnjanestreet-status-string)))
  (or nnjanestreet--state (error "No Jane Street server selected")))

(defun nnjanestreet--group (db name &optional create)
  "Find NAME in DB, optionally CREATE it."
  (or (cl-find name (nnjanestreet--db-groups db)
               :key (lambda (item) (plist-get item :name)) :test #'equal)
      (when create
        (unless (equal name "posts") (error "Expected posts group"))
        (let ((record (list :name name :source "Jane Street"
                            :high 0 :entries nil)))
          (push record (nnjanestreet--db-groups db))
          record))))

(defun nnjanestreet--child (node tag)
  "Return NODE's first direct child named TAG."
  (seq-find (lambda (child)
              (and (listp child) (eq (dom-tag child) tag)))
            (dom-children node)))

(defun nnjanestreet--text (node)
  "Return trimmed text from DOM NODE."
  (when node (string-trim (dom-inner-text node))))

(defun nnjanestreet--children-html (node)
  "Serialize the children of DOM NODE as HTML."
  (mapconcat
   (lambda (child)
     (if (stringp child)
         (xml-escape-string child)
       (with-temp-buffer
         (dom-print child)
         (buffer-string))))
   (dom-children node) ""))

(defun nnjanestreet--parse-feed (xml)
  "Extract the official RSS index from XML."
  (with-temp-buffer
    (insert xml)
    (let ((feed (libxml-parse-xml-region (point-min) (point-max))))
      (unless (eq (dom-tag feed) 'rss)
        (error "Jane Street did not return RSS"))
      (delq nil
            (mapcar
             (lambda (item)
               (let ((link (nnjanestreet--text (nnjanestreet--child item 'link))))
                 (when (and link (string-prefix-p "https://blog.janestreet.com/" link))
                   (list :guid link :link link
                         :title (nnjanestreet--text (nnjanestreet--child item 'title))
                         :date (nnjanestreet--text (nnjanestreet--child item 'pubDate))
                         :author "Jane Street"
                         :body (or (nnjanestreet--text
                                    (nnjanestreet--child item 'description)) "")
                         :full nil))))
             (dom-by-tag feed 'item))))))

(defun nnjanestreet--post (html url)
  "Extract full article HTML and author names from HTML at URL.
Exclude navigation, recommendations and author avatars.  Resolve relative
links and image sources so SHR can display the standalone article."
  (with-temp-buffer
    (insert html)
    (let* ((document (libxml-parse-html-region (point-min) (point-max)))
           (article (seq-find (lambda (node) (dom-by-class node "post-content"))
                              (dom-by-tag document 'article)))
           (post (car (dom-by-class article "post-content")))
           (hero (car (dom-by-class article "featimg-wrapper")))
           (authors (mapcar #'nnjanestreet--text
                           (cl-mapcan (lambda (node) (dom-by-tag node 'a))
                                      (dom-by-class article "name")))))
      (unless post (error "Jane Street page has no post-content"))
      (dolist (node (append (dom-by-tag post 'script) (dom-by-tag post 'style)))
        (dom-remove-node post node))
      (dolist (root (delq nil (list hero post)))
        (dolist (tag '(a img source video))
          (dolist (node (dom-by-tag root tag))
            (dolist (attribute '(href src poster))
              (when-let* ((value (dom-attr node attribute)))
                (dom-set-attribute node attribute (url-expand-file-name value url)))))))
      (list :body (concat (when hero (nnjanestreet--children-html hero))
                          (nnjanestreet--children-html post))
            :author (if authors (string-join authors ", ") "Jane Street")))))

(defun nnjanestreet--request-text (url)
  "GET URL and return its decoded response body."
  (with-current-buffer (or (url-retrieve-synchronously
                            url t t nnjanestreet-request-timeout)
                           (error "Could not fetch %s" url))
    (unwind-protect
        (progn
          (goto-char (point-min))
          (unless (looking-at "HTTP/[0-9.]+ 2[0-9][0-9]")
            (error "Jane Street request failed: %s"
                   (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
          (unless (re-search-forward "\r?\n\r?\n" nil t)
            (error "Jane Street response has no body"))
          (decode-coding-string
           (buffer-substring-no-properties (point) (point-max))
           'utf-8))
      (kill-buffer (current-buffer)))))

(defun nnjanestreet--fetch (_record)
  "Fetch the official Jane Street article index."
  (nnjanestreet--parse-feed
   (nnjanestreet--request-text "https://blog.janestreet.com/feed.xml")))

(defun nnjanestreet--ensure-full-body (db entry)
  "Fetch and persist ENTRY's full HTML and author on first open.
A failed request leaves its snapshot untouched and can be retried."
  (unless (plist-get entry :full)
    (let* ((post (nnjanestreet--post
                  (nnjanestreet--request-text (plist-get entry :link))
                  (plist-get entry :link)))
           (body (plist-get post :body)))
      (when (string-empty-p (string-trim body))
        (error "Jane Street returned an empty article"))
      (setf (plist-get entry :body) body
            (plist-get entry :author) (plist-get post :author)
            (plist-get entry :full) t)
      (nnjanestreet--save db)))
  entry)

(defun nnjanestreet--refresh (db record)
  "Merge current posts into RECORD, preserving stable article numbers."
  (let ((entries (plist-get record :entries))
        (high (plist-get record :high)))
    (dolist (item (nnjanestreet--fetch record))
      (let ((old (cl-find (plist-get item :guid) entries
                          :key (lambda (entry) (plist-get entry :guid))
                          :test #'equal)))
        (if old
            (progn
              (setq item (plist-put item :number (plist-get old :number)))
              (when (plist-get old :full)
                (setf (plist-get item :body) (plist-get old :body)
                      (plist-get item :author) (plist-get old :author)
                      (plist-get item :full) t))
              (setq entries (cons item (delq old entries))))
          (setq high (1+ high)
                item (plist-put item :number high))
          (push item entries))))
    (setf (plist-get record :entries) entries
          (plist-get record :high) high)
    (nnjanestreet--save db)))

(defun nnjanestreet--message-id (entry)
  "Stable Message-ID for ENTRY."
  (format "<%s@janestreet.invalid>"
          (secure-hash 'sha256 (plist-get entry :guid))))

(defun nnjanestreet--entry (record article)
  "Find ARTICLE by number or Message-ID in RECORD."
  (cl-find-if
   (lambda (entry)
     (if (integerp article)
         (= article (plist-get entry :number))
       (equal article (nnjanestreet--message-id entry))))
   (plist-get record :entries)))

(defun nnjanestreet--header (entry)
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
   (nnjanestreet--message-id entry) "" 0 0 "" nil))

(deffoo nnjanestreet-request-create-group (group &optional server _args)
  (let ((db (nnjanestreet--select server)))
    (nnjanestreet--group db group t)
    (nnjanestreet--save db)
    t))
(deffoo nnjanestreet-close-group (_group &optional _server) t)
(deffoo nnjanestreet-asynchronous-p () nil)
(deffoo nnjanestreet-request-post (&optional _server)
  (nnheader-report 'nnjanestreet "Jane Street posts are read-only here"))

(deffoo nnjanestreet-request-scan (&optional group server)
  (condition-case problem
      (let ((db (nnjanestreet--select server)))
        (dolist (record (if group (list (nnjanestreet--group db group))
                          (nnjanestreet--db-groups db)))
          (when record (nnjanestreet--refresh db record)))
        t)
    (error (nnheader-report 'nnjanestreet "%s" (error-message-string problem)))))

(deffoo nnjanestreet-request-group (group &optional server _fast _info)
  (if-let* ((record (nnjanestreet--group (nnjanestreet--select server) group)))
      (nnheader-insert "211 %d 1 %d %s\n" (length (plist-get record :entries))
                       (plist-get record :high) group t)
    (nnheader-report 'nnjanestreet "Unknown Jane Street subscription")))

(deffoo nnjanestreet-request-list (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (record (nnjanestreet--db-groups (nnjanestreet--select server)))
      (insert (format "%s %d 1 n\n" (plist-get record :name)
                      (plist-get record :high)))))
  t)
(deffoo nnjanestreet-retrieve-groups (_groups &optional server)
  (nnjanestreet-request-list server) 'active)
(deffoo nnjanestreet-request-list-newsgroups (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (record (nnjanestreet--db-groups (nnjanestreet--select server)))
      (insert (plist-get record :name) "\tJane Street · "
              (plist-get record :source) "\n")))
  t)

(deffoo nnjanestreet-retrieve-headers (articles &optional group server _fetch-old)
  (let ((record (nnjanestreet--group (nnjanestreet--select server) group)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (number articles)
        (when-let* ((entry (and record (nnjanestreet--entry record number))))
          (nnheader-insert-nov (nnjanestreet--header entry))))))
  'nov)

(deffoo nnjanestreet-request-article (article &optional group server buffer)
  (let* ((db (nnjanestreet--select server))
         (record (nnjanestreet--group db group))
         (entry (and record (nnjanestreet--entry record article))))
    (if (not entry)
        (nnheader-report 'nnjanestreet "Article is absent from the snapshot")
      (nnjanestreet--ensure-full-body db entry)
      (let ((header (nnjanestreet--header entry)))
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
(defun nnjanestreet-subscribe ()
  "Subscribe to Jane Street blog posts using native Gnus group state."
  (interactive)
  (unless (gnus-alive-p) (gnus-no-server))
  (let* ((server "blog.janestreet.com")
         (method `(nnjanestreet ,server))
         (group "posts")
         (full (gnus-group-prefixed-name group method)))
    (unless (nnjanestreet-open-server server)
      (error "%s" nnjanestreet-status-string))
    (with-current-buffer gnus-group-buffer
      (unless (gnus-group-entry full) (gnus-group-make-group group method))
      (gnus-group-change-level full gnus-level-default-subscribed))
    (unless (nnjanestreet-request-scan group server)
      (error "%s" nnjanestreet-status-string))
    (with-current-buffer gnus-group-buffer
      (gnus-group-read-group t t full))))

(provide 'nnjanestreet)
;;; nnjanestreet.el ends here
