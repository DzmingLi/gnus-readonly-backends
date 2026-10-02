;;; nntheatlantic.el --- Atlantic author feeds in Gnus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: news

;;; Commentary:

;; Each explicitly subscribed Atlantic author is a read-only Gnus group.
;; The official full-content Atom feed supplies articles and lead images.
;; Gnus owns reading marks; this backend persists stable article numbers and
;; raw feed content so display options also apply to cached articles.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'json)
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

(defgroup nntheatlantic nil "The Atlantic in Gnus." :group 'gnus)
(defcustom nntheatlantic-show-recirculation-links nil
  "Whether to show The Atlantic's injected in-article recommendations.
When nil, paragraphs marked data-id=\"injected-recirculation-link\" are
removed from the article body.  This also applies to cached articles."
  :type 'boolean :group 'nntheatlantic)
(defcustom nntheatlantic-request-timeout 30
  "Seconds to wait for an author feed."
  :type 'number :group 'nntheatlantic)

(nnoo-declare nntheatlantic)
(defvoo nntheatlantic-directory (expand-file-name "nntheatlantic/" gnus-directory)
  "Directory for stable article snapshots.")
(defvoo nntheatlantic--state nil)
(defvoo nntheatlantic-status-string "")
(nnoo-define-basics nntheatlantic)
(cl-defstruct nntheatlantic--db file groups)
(defvar nntheatlantic--databases (make-hash-table :test #'equal))

(defun nntheatlantic--load (file)
  "Read FILE as a JSON snapshot without evaluating code."
  (let ((db (make-nntheatlantic--db :file file)))
    (when (file-readable-p file)
      (let ((data (with-temp-buffer
                    (insert-file-contents file)
                    (json-parse-buffer :object-type 'plist :array-type 'list))))
        (unless (equal (plist-get data :version) 1)
          (error "Unsupported Atlantic snapshot"))
        (setf (nntheatlantic--db-groups db) (plist-get data :groups))))
    db))

(defun nntheatlantic--save (db)
  "Atomically save DB without Gnus reading marks."
  (let* ((file (nntheatlantic--db-file db))
         (directory (file-name-directory file)) temp)
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temp (make-temp-file (expand-file-name ".snapshot-" directory)))
          (set-file-modes temp #o600)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file temp
              (insert (json-serialize
                       (list :version 1
                             :groups (vconcat
                                      (mapcar
                                       (lambda (group)
                                         (let ((copy (copy-sequence group)))
                                           (setf (plist-get copy :entries)
                                                 (vconcat (plist-get copy :entries)))
                                           copy))
                                       (nntheatlantic--db-groups db))))))))
          (rename-file temp file t))
      (when (and temp (file-exists-p temp)) (delete-file temp)))))

(deffoo nntheatlantic-open-server (server &optional defs _connectionless)
  (condition-case problem
      (progn
        (nnoo-change-server 'nntheatlantic server defs)
        (let ((file (expand-file-name
                     (concat (secure-hash 'sha256 server) ".json")
                     nntheatlantic-directory)))
          (setq nntheatlantic--state
                (or (gethash file nntheatlantic--databases)
                    (puthash file (nntheatlantic--load file)
                             nntheatlantic--databases))))
        t)
    (error (nnheader-report 'nntheatlantic "%s" (error-message-string problem)))))

(defun nntheatlantic--select (&optional server)
  "Return SERVER's state."
  (when server
    (unless (nntheatlantic-open-server server)
      (error "%s" nntheatlantic-status-string)))
  (or nntheatlantic--state (error "No Atlantic server selected")))

(defun nntheatlantic--slug (group)
  "Get an author slug from GROUP."
  (unless (and (stringp group)
               (string-match "\\`author\\.\\([[:alnum:]-]+\\)\\'" group))
    (error "Expected author.SLUG"))
  (match-string 1 group))

(defun nntheatlantic--group (db name &optional create)
  "Find NAME in DB, optionally CREATE it."
  (or (cl-find name (nntheatlantic--db-groups db)
               :key (lambda (item) (plist-get item :name)) :test #'equal)
      (when create
        (let ((record (list :name name :slug (nntheatlantic--slug name)
                            :high 0 :entries nil)))
          (push record (nntheatlantic--db-groups db))
          record))))

(defun nntheatlantic--child (node tag)
  "Return the first direct child of NODE named TAG."
  (cl-find-if (lambda (child) (and (listp child) (eq (dom-tag child) tag)))
              (dom-children node)))

(defun nntheatlantic--text (node)
  "Return NODE's trimmed text."
  (when node (string-trim (dom-inner-text node))))

(defun nntheatlantic--parse (xml)
  "Parse Atlantic Atom XML into feed entries."
  (with-temp-buffer
    ;; `url-retrieve-synchronously' supplies raw, unibyte XML.  Keep the
    ;; input unibyte so libxml honors its UTF-8 declaration on large feeds.
    (set-buffer-multibyte nil)
    (insert (if (multibyte-string-p xml)
                (encode-coding-string xml 'utf-8) xml))
    (let ((root (libxml-parse-xml-region (point-min) (point-max))))
      (mapcar
       (lambda (node)
         (let* ((author (nntheatlantic--child node 'author))
                (image (cl-find-if
                        (lambda (child) (dom-attr child 'url))
                        (dom-by-tag node 'content)))
                (content (cl-find-if
                          (lambda (child) (equal (dom-attr child 'type) "html"))
                          (dom-by-tag node 'content)))
                (link (cl-find-if
                       (lambda (child) (equal (dom-attr child 'rel) "alternate"))
                       (dom-by-tag node 'link))))
           (list :guid (nntheatlantic--text (nntheatlantic--child node 'id))
                 :title (nntheatlantic--text (nntheatlantic--child node 'title))
                 :author (nntheatlantic--text (and author (nntheatlantic--child author 'name)))
                 :date (nntheatlantic--text
                        (or (nntheatlantic--child node 'published)
                            (nntheatlantic--child node 'updated)))
                 :link (and link (dom-attr link 'href))
                 :image (and image (dom-attr image 'url))
                 :body (and content (dom-inner-text content)))))
       (dom-by-tag root 'entry)))))

(defun nntheatlantic--fetch (slug)
  "Fetch the official full-content author feed for SLUG."
  (let ((url (format "https://www.theatlantic.com/feed/author/%s/" slug)))
    (with-current-buffer (or (url-retrieve-synchronously
                              url t t nntheatlantic-request-timeout)
                             (error "Could not fetch %s" url))
      (unwind-protect
          (progn
            (goto-char (point-min))
            (unless (looking-at "HTTP/[0-9.]+ 2[0-9][0-9]")
              (error "Atlantic feed request failed"))
            (unless (re-search-forward "\r?\n\r?\n" nil t)
              (error "Atlantic feed response has no body"))
            (nntheatlantic--parse (buffer-substring-no-properties (point) (point-max))))
        (kill-buffer (current-buffer))))))

(defun nntheatlantic--refresh (db record)
  "Merge the latest author feed into RECORD in DB."
  (let ((entries (plist-get record :entries))
        (high (plist-get record :high)))
    (dolist (item (nntheatlantic--fetch (plist-get record :slug)))
      (when-let* ((guid (plist-get item :guid)))
        (let ((old (cl-find guid entries :key (lambda (e) (plist-get e :guid))
                            :test #'equal)))
          (if old
              (progn
                (setq item (plist-put item :number (plist-get old :number)))
                (setq entries (cons item (delq old entries))))
            (setq high (1+ high)
                  item (plist-put item :number high))
            (push item entries)))))
    (setf (plist-get record :entries) entries
          (plist-get record :high) high)
    (nntheatlantic--save db)))

(defun nntheatlantic--strip-recirculation (html)
  "Remove publisher-injected recommendation paragraphs from HTML."
  (with-temp-buffer
    (insert (or html ""))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward
              "<p\\b[^>]*\\bdata-id=[\"']injected-recirculation-link[\"'][^>]*>"
              nil t)
        (let ((start (match-beginning 0)))
          (if (search-forward "</p>" nil t)
              (delete-region start (point))
            (goto-char (point-max)))))
    (buffer-string))))

(defun nntheatlantic--render (entry)
  "Render ENTRY as a complete HTML article."
  (let* ((title (xml-escape-string (or (plist-get entry :title) "")))
         (image (plist-get entry :image))
         (body (or (plist-get entry :body) "")))
    (concat "<h1>" title "</h1>"
            (when image
              (format "<figure><img src=\"%s\" alt=\"%s\"></figure>"
                      (xml-escape-string image) title))
            (replace-regexp-in-string
             "<span class=\"smallcaps\">\\([^<]*\\)</span>" "<strong>\\1</strong>"
             (if nntheatlantic-show-recirculation-links
                 body (nntheatlantic--strip-recirculation body)) t))))

(defun nntheatlantic--entry (record article)
  "Find ARTICLE in RECORD by number or Message-ID."
  (cl-find-if
   (lambda (entry)
     (if (integerp article)
         (= article (plist-get entry :number))
       (equal article (nntheatlantic--message-id entry))))
   (plist-get record :entries)))

(defun nntheatlantic--message-id (entry)
  "Stable Message-ID for ENTRY."
  (format "<%s@theatlantic.invalid>"
          (secure-hash 'sha256 (plist-get entry :guid))))

(defun nntheatlantic--header (entry)
  "Construct Gnus mail header for ENTRY."
  (make-full-mail-header
   (plist-get entry :number)
   (replace-regexp-in-string "[\r\n]+" " " (or (plist-get entry :title) ""))
   (replace-regexp-in-string "[\r\n]+" " " (or (plist-get entry :author) "The Atlantic"))
   (condition-case nil
       (let ((system-time-locale "C"))
         (format-time-string "%a, %d %b %Y %T %z"
                             (date-to-time (plist-get entry :date)) t))
     (error "Thu, 01 Jan 1970 00:00:00 +0000"))
   (nntheatlantic--message-id entry) "" 0 0 "" nil))

(deffoo nntheatlantic-request-create-group (group &optional server _args)
  (let ((db (nntheatlantic--select server)))
    (nntheatlantic--group db group t)
    (nntheatlantic--save db)
    t))
(deffoo nntheatlantic-close-group (_group &optional _server) t)
(deffoo nntheatlantic-asynchronous-p () nil)
(deffoo nntheatlantic-request-post (&optional _server)
  (nnheader-report 'nntheatlantic "The Atlantic author feed is read-only"))

(deffoo nntheatlantic-request-scan (&optional group server)
  (condition-case problem
      (let ((db (nntheatlantic--select server)))
        (dolist (record (if group (list (nntheatlantic--group db group))
                          (nntheatlantic--db-groups db)))
          (when record (nntheatlantic--refresh db record)))
        t)
    (error (nnheader-report 'nntheatlantic "%s" (error-message-string problem)))))

(deffoo nntheatlantic-request-group (group &optional server _fast _info)
  (if-let* ((record (nntheatlantic--group (nntheatlantic--select server) group)))
      (nnheader-insert "211 %d 1 %d %s\n" (length (plist-get record :entries))
                       (plist-get record :high) group t)
    (nnheader-report 'nntheatlantic "Unknown author subscription")))

(deffoo nntheatlantic-request-list (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (record (nntheatlantic--db-groups (nntheatlantic--select server)))
      (insert (format "%s %d 1 n\n" (plist-get record :name)
                      (plist-get record :high)))))
  t)
(deffoo nntheatlantic-retrieve-groups (_groups &optional server)
  (nntheatlantic-request-list server) 'active)
(deffoo nntheatlantic-request-list-newsgroups (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (record (nntheatlantic--db-groups (nntheatlantic--select server)))
      (insert (plist-get record :name) "\tThe Atlantic: "
              (plist-get record :slug) "\n")))
  t)

(deffoo nntheatlantic-retrieve-headers (articles &optional group server _fetch-old)
  (let ((record (nntheatlantic--group (nntheatlantic--select server) group)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (number articles)
        (when-let* ((entry (and record (nntheatlantic--entry record number))))
          (nnheader-insert-nov (nntheatlantic--header entry))))))
  'nov)

(deffoo nntheatlantic-request-article (article &optional group server buffer)
  (let* ((record (nntheatlantic--group (nntheatlantic--select server) group))
         (entry (and record (nntheatlantic--entry record article))))
    (if (not entry)
        (nnheader-report 'nntheatlantic "Article is absent from the snapshot")
      (let ((header (nntheatlantic--header entry)))
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
                   (encode-coding-string (nntheatlantic--render entry) 'utf-8)) "\n")))
      (cons group (plist-get entry :number)))))

;;;###autoload
(defun nntheatlantic-subscribe-author (slug)
  "Subscribe to The Atlantic author SLUG as one Gnus group."
  (interactive "sAtlantic author slug: ")
  (unless (string-match-p "\\`[[:alnum:]-]+\\'" slug)
    (user-error "Enter an author slug, such as ian-bogost"))
  (unless (gnus-alive-p) (gnus-no-server))
  (let* ((server "theatlantic.com")
         (group (concat "author." slug))
         (method `(nntheatlantic ,server))
         (full (gnus-group-prefixed-name group method)))
    (unless (nntheatlantic-open-server server)
      (error "%s" nntheatlantic-status-string))
    (with-current-buffer gnus-group-buffer
      (unless (gnus-group-entry full)
        (gnus-group-make-group group method))
      (gnus-group-change-level full gnus-level-default-subscribed))
    (unless (nntheatlantic-request-scan group server)
      (error "%s" nntheatlantic-status-string))
    (with-current-buffer gnus-group-buffer
      (gnus-group-read-group t t full))))

(provide 'nntheatlantic)
;;; nntheatlantic.el ends here
