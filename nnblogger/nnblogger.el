;;; nnblogger.el --- Full Blogger posts in Gnus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: news

;;; Commentary:

;; Read-only Gnus backend for public Blogger posts.  Each blog host is a
;; group.  Atom supplies article metadata; opening a summary-only article
;; fetches the full post body from its page.  Gnus owns read and tick marks.

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

(defgroup nnblogger nil "Blogger in Gnus." :group 'gnus)
(defcustom nnblogger-request-timeout 30
  "Seconds to wait for a public Blogger feed or post page."
  :type 'number :group 'nnblogger)
(defcustom nnblogger-feed-limit 25
  "Maximum number of recent posts to request from a Blogger feed."
  :type 'integer :group 'nnblogger)

(nnoo-declare nnblogger)
(defvoo nnblogger-directory (expand-file-name "nnblogger/" gnus-directory)
  "Directory for local post snapshots and stable article numbers.")
(defvoo nnblogger--state nil)
(defvoo nnblogger-status-string "")
(nnoo-define-basics nnblogger)
(cl-defstruct nnblogger--db file groups)
(defvar nnblogger--databases (make-hash-table :test #'equal))

(defun nnblogger--load (file)
  "Read FILE as JSON without evaluating code."
  (let ((db (make-nnblogger--db :file file)))
    (when (file-readable-p file)
      (let ((data (with-temp-buffer
                    (insert-file-contents file)
                    (json-parse-buffer :object-type 'plist :array-type 'list
                                       :null-object nil :false-object nil))))
        (unless (equal (plist-get data :version) 1)
          (error "Unsupported Blogger snapshot"))
        (setf (nnblogger--db-groups db) (plist-get data :groups))))
    db))

(defun nnblogger--save (db)
  "Atomically save DB without Gnus reading marks."
  (let* ((file (nnblogger--db-file db))
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
                        (nnblogger--db-groups db))))))))
          (rename-file temp file t))
      (when (and temp (file-exists-p temp)) (delete-file temp)))))

(deffoo nnblogger-open-server (server &optional defs _connectionless)
  (condition-case problem
      (progn
        (nnoo-change-server 'nnblogger server defs)
        (let ((file (expand-file-name
                     (concat (secure-hash 'sha256 server) ".json")
                     nnblogger-directory)))
          (setq nnblogger--state
                (or (gethash file nnblogger--databases)
                    (puthash file (nnblogger--load file) nnblogger--databases))))
        t)
    (error (nnheader-report 'nnblogger "%s" (error-message-string problem)))))

(defun nnblogger--select (&optional server)
  "Return SERVER's local database."
  (when server
    (unless (nnblogger-open-server server) (error "%s" nnblogger-status-string)))
  (or nnblogger--state (error "No Blogger server selected")))

(defun nnblogger--host (group)
  "Extract and validate the blog host from GROUP."
  (unless (and (stringp group)
               (string-match
                "\\`posts\\.\\([[:alnum:]][[:alnum:].-]*\\.[[:alpha:]][[:alnum:]-]*\\)\\'"
                group))
    (error "Expected posts.BLOG-HOST"))
  (match-string 1 group))

(defun nnblogger--group (db name &optional create)
  "Find NAME in DB, optionally CREATE it."
  (or (cl-find name (nnblogger--db-groups db)
               :key (lambda (item) (plist-get item :name)) :test #'equal)
      (when create
        (let ((record (list :name name :host (nnblogger--host name)
                            :high 0 :entries nil)))
          (push record (nnblogger--db-groups db))
          record))))

(defun nnblogger--child (node tag)
  "Return NODE's first direct child named TAG."
  (seq-find (lambda (child)
              (and (listp child) (eq (dom-tag child) tag)))
            (dom-children node)))

(defun nnblogger--text (node)
  "Return trimmed text from DOM NODE."
  (when node (string-trim (dom-inner-text node))))

(defun nnblogger--children-html (node)
  "Serialize the children of DOM NODE as HTML."
  (mapconcat
   (lambda (child)
     (if (stringp child)
         (xml-escape-string child)
       (with-temp-buffer
         (dom-print child)
         (buffer-string))))
   (dom-children node) ""))

(defun nnblogger--feed-html (node)
  "Return HTML from an Atom content or summary NODE."
  (when node
    (let ((kind (dom-attr node 'type)))
      (pcase kind
        ("html" (nnblogger--text node))
        ("xhtml" (nnblogger--children-html node))
        (_ (when-let* ((plain (nnblogger--text node)))
             (format "<p>%s</p>" (xml-escape-string plain))))))))

(defun nnblogger--entry-link (entry)
  "Return the alternate article URL from Atom ENTRY."
  (when-let* ((link (seq-find
                     (lambda (node)
                       (equal (dom-attr node 'rel) "alternate"))
                     (dom-by-tag entry 'link))))
    (dom-attr link 'href)))

(defun nnblogger--parse-entry (entry)
  "Convert Atom ENTRY into a Gnus article record."
  (let* ((id (nnblogger--text (nnblogger--child entry 'id)))
         (link (nnblogger--entry-link entry))
         (author-node (nnblogger--child entry 'author))
         (content (nnblogger--child entry 'content))
         (summary (nnblogger--child entry 'summary))
         (body (or (nnblogger--feed-html content)
                   (nnblogger--feed-html summary)
                   "")))
    (when (and id link
               (or (string-prefix-p "https://" link)
                   (string-prefix-p "http://" link)))
      (list :guid id
            :title (or (nnblogger--text (nnblogger--child entry 'title))
                       "Untitled")
            :link link
            :date (or (nnblogger--text (nnblogger--child entry 'published))
                      (nnblogger--text (nnblogger--child entry 'updated)))
            :updated (nnblogger--text (nnblogger--child entry 'updated))
            :author (or (nnblogger--text
                         (and author-node (nnblogger--child author-node 'name)))
                        "Blogger")
            :body body
            :full (and content (not (string-empty-p body)))))))

(defun nnblogger--parse-feed (xml)
  "Extract post records from Blogger Atom XML."
  (with-temp-buffer
    (insert xml)
    (let* ((feed (libxml-parse-xml-region (point-min) (point-max)))
           (entries (mapcar #'nnblogger--parse-entry
                            (dom-by-tag feed 'entry))))
      (unless (eq (dom-tag feed) 'feed)
        (error "Blogger did not return an Atom feed"))
      (delq nil entries))))

(defun nnblogger--post-html (html)
  "Extract full Blogger post body from HTML."
  (with-temp-buffer
    (insert html)
    (let* ((document (libxml-parse-html-region (point-min) (point-max)))
           (post (car (dom-by-class document "post-body"))))
      (when post
        (string-trim (nnblogger--children-html post))))))

(defun nnblogger--request-text (url)
  "GET URL and return its decoded response body."
  (with-current-buffer (or (url-retrieve-synchronously
                            url t t nnblogger-request-timeout)
                           (error "Could not fetch %s" url))
    (unwind-protect
        (progn
          (goto-char (point-min))
          (unless (looking-at "HTTP/[0-9.]+ 2[0-9][0-9]")
            (error "Blogger request failed: %s"
                   (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
          (unless (re-search-forward "\r?\n\r?\n" nil t)
            (error "Blogger response has no body"))
          (decode-coding-string
           (buffer-substring-no-properties (point) (point-max))
           'utf-8))
      (kill-buffer (current-buffer)))))

(defun nnblogger--fetch (record)
  "Fetch the recent Atom entries for RECORD."
  (nnblogger--parse-feed
   (nnblogger--request-text
    (format "https://%s/feeds/posts/default?max-results=%d"
            (plist-get record :host)
            (max 1 nnblogger-feed-limit)))))

(defun nnblogger--ensure-full-body (db entry)
  "Fetch ENTRY's full HTML if its feed supplied only a summary."
  (unless (plist-get entry :full)
    (condition-case problem
        (when-let* ((body (nnblogger--post-html
                          (nnblogger--request-text (plist-get entry :link)))))
          (unless (string-empty-p body)
            (setf (plist-get entry :body) body
                  (plist-get entry :full) t)
            (nnblogger--save db)))
      (error (message "Blogger full post unavailable: %s"
                      (error-message-string problem)))))
  entry)

(defun nnblogger--refresh (db record)
  "Merge current posts into RECORD, preserving stable article numbers."
  (let ((entries (plist-get record :entries))
        (high (plist-get record :high)))
    (dolist (item (nnblogger--fetch record))
      (let ((old (cl-find (plist-get item :guid) entries
                          :key (lambda (entry) (plist-get entry :guid))
                          :test #'equal)))
        (if old
            (progn
              (setq item (plist-put item :number (plist-get old :number)))
              (when (and (plist-get old :full)
                         (equal (plist-get old :updated)
                                (plist-get item :updated)))
                (setf (plist-get item :body) (plist-get old :body)
                      (plist-get item :full) t))
              (setq entries (cons item (delq old entries))))
          (setq high (1+ high)
                item (plist-put item :number high))
          (push item entries))))
    (setf (plist-get record :entries) entries
          (plist-get record :high) high)
    (nnblogger--save db)))

(defun nnblogger--message-id (entry)
  "Stable Message-ID for ENTRY."
  (format "<%s@blogger.invalid>" (plist-get entry :guid)))

(defun nnblogger--entry (record article)
  "Find ARTICLE by number or Message-ID in RECORD."
  (cl-find-if
   (lambda (entry)
     (if (integerp article)
         (= article (plist-get entry :number))
       (equal article (nnblogger--message-id entry))))
   (plist-get record :entries)))

(defun nnblogger--header (entry)
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
   (nnblogger--message-id entry) "" 0 0 "" nil))

(deffoo nnblogger-request-create-group (group &optional server _args)
  (let ((db (nnblogger--select server)))
    (nnblogger--group db group t)
    (nnblogger--save db)
    t))
(deffoo nnblogger-close-group (_group &optional _server) t)
(deffoo nnblogger-asynchronous-p () nil)
(deffoo nnblogger-request-post (&optional _server)
  (nnheader-report 'nnblogger "Blogger posts are read-only here"))

(deffoo nnblogger-request-scan (&optional group server)
  (condition-case problem
      (let ((db (nnblogger--select server)))
        (dolist (record (if group (list (nnblogger--group db group))
                          (nnblogger--db-groups db)))
          (when record (nnblogger--refresh db record)))
        t)
    (error (nnheader-report 'nnblogger "%s" (error-message-string problem)))))

(deffoo nnblogger-request-group (group &optional server _fast _info)
  (if-let* ((record (nnblogger--group (nnblogger--select server) group)))
      (nnheader-insert "211 %d 1 %d %s\n" (length (plist-get record :entries))
                       (plist-get record :high) group t)
    (nnheader-report 'nnblogger "Unknown Blogger subscription")))

(deffoo nnblogger-request-list (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (record (nnblogger--db-groups (nnblogger--select server)))
      (insert (format "%s %d 1 n\n" (plist-get record :name)
                      (plist-get record :high)))))
  t)
(deffoo nnblogger-retrieve-groups (_groups &optional server)
  (nnblogger-request-list server) 'active)
(deffoo nnblogger-request-list-newsgroups (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (record (nnblogger--db-groups (nnblogger--select server)))
      (insert (plist-get record :name) "\tBlogger · "
              (plist-get record :host) "\n")))
  t)

(deffoo nnblogger-retrieve-headers (articles &optional group server _fetch-old)
  (let ((record (nnblogger--group (nnblogger--select server) group)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (number articles)
        (when-let* ((entry (and record (nnblogger--entry record number))))
          (nnheader-insert-nov (nnblogger--header entry))))))
  'nov)

(deffoo nnblogger-request-article (article &optional group server buffer)
  (let* ((db (nnblogger--select server))
         (record (nnblogger--group db group))
         (entry (and record (nnblogger--entry record article))))
    (if (not entry)
        (nnheader-report 'nnblogger "Article is absent from the snapshot")
      (nnblogger--ensure-full-body db entry)
      (let ((header (nnblogger--header entry)))
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
(defun nnblogger-subscribe-blog (host)
  "Subscribe to Blogger HOST's posts in Gnus."
  (interactive "sBlogger host (e.g. example.blogspot.com): ")
  (setq host (downcase (string-trim host)))
  (unless (gnus-alive-p) (gnus-no-server))
  (let* ((server "blogger")
         (method `(nnblogger ,server))
         (group (concat "posts." host))
         (full (gnus-group-prefixed-name group method)))
    (nnblogger--host group)
    (unless (nnblogger-open-server server) (error "%s" nnblogger-status-string))
    (with-current-buffer gnus-group-buffer
      (unless (gnus-group-entry full) (gnus-group-make-group group method))
      (gnus-group-change-level full gnus-level-default-subscribed))
    (unless (nnblogger-request-scan group server)
      (error "%s" nnblogger-status-string))
    (with-current-buffer gnus-group-buffer
      (gnus-group-read-group t t full))))

(provide 'nnblogger)
;;; nnblogger.el ends here
