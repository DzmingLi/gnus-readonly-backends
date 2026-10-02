;;; nnzhihu.el --- Zhihu articles and answers in Gnus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (zhihu "0.1.0") (firefox-cookies "0.1.0"))
;; Keywords: news

;;; Commentary:

;; Read-only Gnus groups for a Zhihu person's or organization's articles and
;; a person's answers.  The maintained zhihu.el package signs API requests;
;; Firefox cookies supply authentication.  Cached entries have stable Gnus
;; article numbers while Gnus itself owns read and tick marks.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'zhihu)
(require 'firefox-cookies)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-start)
(require 'nnoo)
(require 'nnheader)
(require 'rfc2047)
(require 'mail-parse)

(defgroup nnzhihu nil "Zhihu in Gnus." :group 'gnus)
(defcustom nnzhihu-cookie-function #'firefox-cookies-get
  "Function returning Zhihu cookies for an absolute URL.
The result is an ordered alist of (NAME . VALUE) strings."
  :type 'function :group 'nnzhihu)
(defcustom nnzhihu-request-timeout 30
  "Seconds to wait for a Zhihu API response."
  :type 'number :group 'nnzhihu)

(nnoo-declare nnzhihu)
(defvoo nnzhihu-directory (expand-file-name "nnzhihu/" gnus-directory)
  "Directory for cached Zhihu entries and stable article numbers.")
(defvoo nnzhihu--state nil)
(defvoo nnzhihu-status-string "")
(nnoo-define-basics nnzhihu)
(cl-defstruct nnzhihu--db file groups)
(defvar nnzhihu--databases (make-hash-table :test #'equal))

(defun nnzhihu--load (file)
  "Read FILE as JSON without evaluating code."
  (let ((db (make-nnzhihu--db :file file)))
    (when (file-readable-p file)
      (let ((data (with-temp-buffer
                    (insert-file-contents file)
                    (json-parse-buffer :object-type 'plist :array-type 'list
                                       :null-object nil :false-object nil))))
        (unless (equal (plist-get data :version) 1)
          (error "Unsupported Zhihu snapshot"))
        (setf (nnzhihu--db-groups db) (plist-get data :groups))))
    db))

(defun nnzhihu--save (db)
  "Atomically save DB without Gnus reading marks or credentials."
  (let* ((file (nnzhihu--db-file db))
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
                        (nnzhihu--db-groups db))))))))
          (rename-file temp file t))
      (when (and temp (file-exists-p temp)) (delete-file temp)))))

(deffoo nnzhihu-open-server (server &optional defs _connectionless)
  (condition-case problem
      (progn
        (nnoo-change-server 'nnzhihu server defs)
        (let ((file (expand-file-name
                     (concat (secure-hash 'sha256 server) ".json")
                     nnzhihu-directory)))
          (setq nnzhihu--state
                (or (gethash file nnzhihu--databases)
                    (puthash file (nnzhihu--load file) nnzhihu--databases))))
        t)
    (error (nnheader-report 'nnzhihu "%s" (error-message-string problem)))))

(defun nnzhihu--select (&optional server)
  "Return SERVER's local database."
  (when server
    (unless (nnzhihu-open-server server) (error "%s" nnzhihu-status-string)))
  (or nnzhihu--state (error "No Zhihu server selected")))

(defun nnzhihu--parts (group)
  "Return (KIND USER-TYPE USER-ID) parsed from GROUP."
  (cond
   ((and (stringp group)
         (string-match "\\`articles\\.\\(people\\|org\\)\\.\\([[:alnum:]_.-]+\\)\\'"
                       group))
    (list 'articles (match-string 1 group) (match-string 2 group)))
   ((and (stringp group)
         (string-match "\\`answers\\.\\([[:alnum:]_.-]+\\)\\'" group))
    (list 'answers "people" (match-string 1 group)))
   (t (error "Use articles.people.USER, articles.org.USER or answers.USER"))))

(defun nnzhihu--group (db name &optional create)
  "Find NAME in DB, optionally CREATE it."
  (or (cl-find name (nnzhihu--db-groups db)
               :key (lambda (item) (plist-get item :name)) :test #'equal)
      (when create
        (pcase-let ((`(,kind ,user-type ,user-id) (nnzhihu--parts name)))
          (let ((record (list :name name :kind (symbol-name kind)
                              :user-type user-type :user-id user-id
                              :high 0 :entries nil)))
            (push record (nnzhihu--db-groups db))
            record)))))

(defun nnzhihu--api-url (record)
  "Return the current item API URL for RECORD."
  (let ((prefix (format "https://www.zhihu.com/api/v4/members/%s/"
                        (plist-get record :user-id))))
    (if (equal (plist-get record :kind) "articles")
        (concat prefix "articles?"
                (url-build-query-string
                 '(("include"
                    "data[*].comment_count,content,voteup_count,created,updated;data[*].author.vip_info")
                   ("offset" "0") ("limit" "20") ("sort_by" "created"))))
      (concat prefix "answers?"
              (url-build-query-string
               '(("limit" "20")
                 ("include" "data[*].is_normal,content")))))))

(defun nnzhihu--headers (url referer)
  "Build cookie and ZSE-signed request headers for URL and REFERER."
  (unless (functionp nnzhihu-cookie-function)
    (user-error "Configure nnzhihu-cookie-function"))
  (let* ((cookies (funcall nnzhihu-cookie-function url))
         (dc0 (cdr (assoc-string "d_c0" cookies)))
         (cookie-header
          (mapconcat (lambda (cookie)
                       (format "%s=%s" (car cookie) (cdr cookie)))
                     cookies "; ")))
    (unless dc0 (error "Zhihu browser profile has no d_c0 cookie"))
    (append `(("Cookie" . ,cookie-header)
              ("Referer" . ,referer)
              ("x-api-version" . "3.0.91")
              ("x-app-za" . "OS=Web")
              ("x-requested-with" . "fetch"))
            (zhihu--zse-request-headers url nil dc0))))

(defun nnzhihu--request-json (url referer)
  "GET URL with REFERER and parse a JSON object."
  (let ((url-request-extra-headers (nnzhihu--headers url referer)))
    (with-current-buffer (or (url-retrieve-synchronously
                              url t t nnzhihu-request-timeout)
                             (error "Could not fetch %s" url))
      (unwind-protect
          (progn
            (goto-char (point-min))
            (unless (looking-at "HTTP/[0-9.]+ 2[0-9][0-9]")
              (error "Zhihu API request failed: %s"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
            (unless (re-search-forward "\r?\n\r?\n" nil t)
              (error "Zhihu API response has no body"))
            (json-parse-buffer :object-type 'plist :array-type 'list
                               :null-object nil :false-object nil))
        (kill-buffer (current-buffer))))))

(defun nnzhihu--clean-content (content)
  "Make Zhihu HTML CONTENT suitable for a Gnus HTML MIME part."
  (let ((result (or content "")))
    (setq result
          (replace-regexp-in-string
           "<noscript\\(?:[[:space:]][^>]*\\)?>.*?</noscript>" "" result t))
    (replace-regexp-in-string
     "\\(?:data-actualsrc\\|data-original\\)=\"\\([^\"]+\\)\""
     "src=\"\\1\"" result t)))

(defun nnzhihu--normalize (item kind)
  "Convert API ITEM of KIND into a persisted article."
  (let* ((id (format "%s" (plist-get item :id)))
         (author (plist-get item :author))
         (question (plist-get item :question))
         (article-p (equal kind "articles")))
    (unless (string-match-p "\\`[0-9]+\\'" id)
      (error "Zhihu item has no numeric ID"))
    (list :guid (concat (if article-p "zhihu-article-" "zhihu-answer-") id)
          :title (or (if article-p (plist-get item :title)
                       (plist-get question :title))
                     (if article-p "知乎文章" "知乎回答"))
          :link (if article-p
                    (format "https://zhuanlan.zhihu.com/p/%s" id)
                  (format "https://www.zhihu.com/question/%s/answer/%s"
                          (plist-get question :id) id))
          :date (or (if article-p (plist-get item :created)
                      (plist-get item :created_time)) 0)
          :author (or (plist-get author :name) "知乎用户")
          :body (nnzhihu--clean-content (plist-get item :content)))))

(defun nnzhihu--fetch (record)
  "Fetch latest articles or answers described by RECORD."
  (let* ((user-type (plist-get record :user-type))
         (user-id (plist-get record :user-id))
         (url (nnzhihu--api-url record))
         (referer (format "https://www.zhihu.com/%s/%s/" user-type user-id))
         (payload (nnzhihu--request-json url referer))
         (data (plist-get payload :data)))
    (unless (listp data) (error "Zhihu API returned no item list"))
    (mapcar (lambda (item) (nnzhihu--normalize item (plist-get record :kind)))
            data)))

(defun nnzhihu--refresh (db record)
  "Merge current source entries into RECORD, preserving article numbers."
  (let ((entries (plist-get record :entries))
        (high (plist-get record :high)))
    (dolist (item (nnzhihu--fetch record))
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
    (nnzhihu--save db)))

(defun nnzhihu--message-id (entry)
  "Stable Message-ID for ENTRY."
  (format "<%s@zhihu.invalid>" (plist-get entry :guid)))

(defun nnzhihu--entry (record article)
  "Find ARTICLE by number or Message-ID in RECORD."
  (cl-find-if
   (lambda (entry)
     (if (integerp article)
         (= article (plist-get entry :number))
       (equal article (nnzhihu--message-id entry))))
   (plist-get record :entries)))

(defun nnzhihu--header (entry)
  "Construct a native Gnus header from ENTRY."
  (make-full-mail-header
   (plist-get entry :number)
   (replace-regexp-in-string "[\r\n]+" " " (plist-get entry :title))
   (replace-regexp-in-string "[\r\n]+" " " (plist-get entry :author))
   (let ((system-time-locale "C"))
     (format-time-string "%a, %d %b %Y %T %z"
                         (seconds-to-time (plist-get entry :date)) t))
   (nnzhihu--message-id entry) "" 0 0 "" nil))

(deffoo nnzhihu-request-create-group (group &optional server _args)
  (let ((db (nnzhihu--select server)))
    (nnzhihu--group db group t)
    (nnzhihu--save db)
    t))
(deffoo nnzhihu-close-group (_group &optional _server) t)
(deffoo nnzhihu-asynchronous-p () nil)
(deffoo nnzhihu-request-post (&optional _server)
  (nnheader-report 'nnzhihu "Zhihu articles and answers are read-only here"))

(deffoo nnzhihu-request-scan (&optional group server)
  (condition-case problem
      (let ((db (nnzhihu--select server)))
        (dolist (record (if group (list (nnzhihu--group db group))
                          (nnzhihu--db-groups db)))
          (when record (nnzhihu--refresh db record)))
        t)
    (error (nnheader-report 'nnzhihu "%s" (error-message-string problem)))))

(deffoo nnzhihu-request-group (group &optional server _fast _info)
  (if-let* ((record (nnzhihu--group (nnzhihu--select server) group)))
      (nnheader-insert "211 %d 1 %d %s\n" (length (plist-get record :entries))
                       (plist-get record :high) group t)
    (nnheader-report 'nnzhihu "Unknown Zhihu subscription")))

(deffoo nnzhihu-request-list (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (record (nnzhihu--db-groups (nnzhihu--select server)))
      (insert (format "%s %d 1 n\n" (plist-get record :name)
                      (plist-get record :high)))))
  t)
(deffoo nnzhihu-retrieve-groups (_groups &optional server)
  (nnzhihu-request-list server) 'active)
(deffoo nnzhihu-request-list-newsgroups (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (record (nnzhihu--db-groups (nnzhihu--select server)))
      (insert (plist-get record :name) "\t知乎 "
              (plist-get record :kind) " · "
              (plist-get record :user-id) "\n")))
  t)

(deffoo nnzhihu-retrieve-headers (articles &optional group server _fetch-old)
  (let ((record (nnzhihu--group (nnzhihu--select server) group)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (number articles)
        (when-let* ((entry (and record (nnzhihu--entry record number))))
          (nnheader-insert-nov (nnzhihu--header entry))))))
  'nov)

(deffoo nnzhihu-request-article (article &optional group server buffer)
  (let* ((record (nnzhihu--group (nnzhihu--select server) group))
         (entry (and record (nnzhihu--entry record article))))
    (if (not entry)
        (nnheader-report 'nnzhihu "Article is absent from the snapshot")
      (let ((header (nnzhihu--header entry)))
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

(defun nnzhihu--subscribe (group)
  "Create GROUP, fetch its current entries and open it in Gnus."
  (unless (gnus-alive-p) (gnus-no-server))
  (let* ((server "zhihu.com")
         (method `(nnzhihu ,server))
         (full (gnus-group-prefixed-name group method)))
    (unless (nnzhihu-open-server server) (error "%s" nnzhihu-status-string))
    (with-current-buffer gnus-group-buffer
      (unless (gnus-group-entry full) (gnus-group-make-group group method))
      (gnus-group-change-level full gnus-level-default-subscribed))
    (unless (nnzhihu-request-scan group server)
      (error "%s" nnzhihu-status-string))
    (with-current-buffer gnus-group-buffer
      (gnus-group-read-group t t full))))

;;;###autoload
(defun nnzhihu-subscribe-articles (user-id &optional organization)
  "Subscribe to USER-ID's articles; with prefix, an ORGANIZATION's articles."
  (interactive (list (read-string "Zhihu user or organization ID: ")
                     current-prefix-arg))
  (unless (string-match-p "\\`[[:alnum:]_.-]+\\'" user-id)
    (user-error "Enter a Zhihu profile ID"))
  (nnzhihu--subscribe
   (format "articles.%s.%s" (if organization "org" "people") user-id)))

;;;###autoload
(defun nnzhihu-subscribe-answers (user-id)
  "Subscribe to Zhihu person USER-ID's answers."
  (interactive "sZhihu person ID: ")
  (unless (string-match-p "\\`[[:alnum:]_.-]+\\'" user-id)
    (user-error "Enter a Zhihu person ID"))
  (nnzhihu--subscribe (concat "answers." user-id)))

(provide 'nnzhihu)
;;; nnzhihu.el ends here
