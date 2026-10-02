;;; nnneteasemusic.el --- NetEase Music user events in Gnus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: news

;;; Commentary:

;; Read-only Gnus backend for public NetEase Music user events.  A user is a
;; group and each event gets a stable article number.  HTML articles include
;; text, pictures, and song cards with a visible play link for text readers.

;;; Code:

(require 'cl-lib)
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

(defgroup nnneteasemusic nil "NetEase Music in Gnus." :group 'gnus)
(defcustom nnneteasemusic-request-timeout 30
  "Seconds to wait for the public NetEase Music API."
  :type 'number :group 'nnneteasemusic)

(nnoo-declare nnneteasemusic)
(defvoo nnneteasemusic-directory
  (expand-file-name "nnneteasemusic/" gnus-directory)
  "Directory for local event snapshots and stable article numbers.")
(defvoo nnneteasemusic--state nil)
(defvoo nnneteasemusic-status-string "")
(nnoo-define-basics nnneteasemusic)
(cl-defstruct nnneteasemusic--db file groups)
(defvar nnneteasemusic--databases (make-hash-table :test #'equal))

(defun nnneteasemusic--load (file)
  "Read FILE as JSON without evaluating code."
  (let ((db (make-nnneteasemusic--db :file file)))
    (when (file-readable-p file)
      (let ((data (with-temp-buffer
                    (insert-file-contents file)
                    (json-parse-buffer :object-type 'plist :array-type 'list
                                       :null-object nil :false-object nil))))
        (unless (equal (plist-get data :version) 1)
          (error "Unsupported NetEase Music snapshot"))
        (setf (nnneteasemusic--db-groups db) (plist-get data :groups))))
    db))

(defun nnneteasemusic--save (db)
  "Atomically save DB without Gnus reading marks."
  (let* ((file (nnneteasemusic--db-file db))
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
                        (nnneteasemusic--db-groups db))))))))
          (rename-file temp file t))
      (when (and temp (file-exists-p temp)) (delete-file temp)))))

(deffoo nnneteasemusic-open-server (server &optional defs _connectionless)
  (condition-case problem
      (progn
        (nnoo-change-server 'nnneteasemusic server defs)
        (let ((file (expand-file-name
                     (concat (secure-hash 'sha256 server) ".json")
                     nnneteasemusic-directory)))
          (setq nnneteasemusic--state
                (or (gethash file nnneteasemusic--databases)
                    (puthash file (nnneteasemusic--load file)
                             nnneteasemusic--databases))))
        t)
    (error (nnheader-report 'nnneteasemusic "%s"
                            (error-message-string problem)))))

(defun nnneteasemusic--select (&optional server)
  "Return SERVER's local database."
  (when server
    (unless (nnneteasemusic-open-server server)
      (error "%s" nnneteasemusic-status-string)))
  (or nnneteasemusic--state (error "No NetEase Music server selected")))

(defun nnneteasemusic--user-id (group)
  "Extract numeric user ID from GROUP."
  (unless (and (stringp group)
               (string-match "\\`events\\.\\([1-9][0-9]*\\)\\'" group))
    (error "Expected events.USER-ID"))
  (match-string 1 group))

(defun nnneteasemusic--group (db name &optional create)
  "Find NAME in DB, optionally CREATE it."
  (or (cl-find name (nnneteasemusic--db-groups db)
               :key (lambda (item) (plist-get item :name)) :test #'equal)
      (when create
        (let ((record (list :name name :user-id (nnneteasemusic--user-id name)
                            :high 0 :entries nil)))
          (push record (nnneteasemusic--db-groups db))
          record))))

(defun nnneteasemusic--https (url)
  "Upgrade protocol-relative or HTTP URL to HTTPS."
  (when url
    (cond
     ((string-prefix-p "//" url) (concat "https:" url))
     ((string-prefix-p "http://" url) (concat "https://" (substring url 7)))
     (t url))))

(defun nnneteasemusic--song-html (song)
  "Render SONG as a player card with a visible play link."
  (when song
    (let* ((id (format "%s" (plist-get song :id)))
           (name (or (plist-get song :name) "网易云单曲"))
           (artists (mapconcat
                     (lambda (artist) (or (plist-get artist :name) ""))
                     (plist-get song :artists) " / "))
           (album-name (plist-get (plist-get song :album) :name))
           (song-url (format "https://music.163.com/#/song?id=%s" id))
           (player-url
            (format "https://music.163.com/outchain/player?type=2&amp;id=%s&amp;auto=0&amp;height=66"
                    id)))
      (concat
       "<div class=\"netease-music-player\">"
       (format "<p><strong>%s</strong>%s%s</p>"
               (xml-escape-string name)
               (if (string-empty-p artists) ""
                 (format " — %s" (xml-escape-string artists)))
               (if album-name
                   (format "<br><small>%s</small>"
                           (xml-escape-string album-name))
                 ""))
       (format "<iframe src=\"%s\" width=\"330\" height=\"86\" frameborder=\"0\"></iframe>"
               player-url)
       (format "<p><a href=\"%s\">▶ 在网易云音乐播放</a></p>"
               (xml-escape-string song-url))
       "</div>"))))

(defun nnneteasemusic--normalize (event fallback-name)
  "Convert EVENT into a Gnus article, using FALLBACK-NAME if needed."
  (let* ((id (format "%s" (plist-get event :id)))
         (user (plist-get event :user))
         (user-id (format "%s" (plist-get user :userId)))
         (nickname (or (plist-get user :nickname) fallback-name))
         (thread (plist-get (plist-get event :info) :commentThread))
         (payload (condition-case nil
                      (json-parse-string
                       (or (plist-get event :json) "{}")
                       :object-type 'plist :array-type 'list
                       :null-object nil :false-object nil)
                    (error nil)))
         (message (or (plist-get payload :msg) ""))
         (song (plist-get payload :song))
         (song-name (plist-get song :name))
         (resource-title (plist-get thread :resourceTitle))
         (pictures (plist-get event :pics)))
    (unless (and (string-match-p "\\`[0-9]+\\'" id)
                 (string-match-p "\\`[0-9]+\\'" user-id))
      (error "NetEase Music event has no numeric ID or user ID"))
    (list
     :guid (concat "netease-event-" id)
     :title (cond
             ((and resource-title song-name
                   (string-match-p (regexp-quote song-name) resource-title))
              resource-title)
             ((and resource-title song-name)
              (format "%s · %s" resource-title song-name))
             (resource-title resource-title)
             ((not (string-empty-p message)) message)
             (song-name song-name)
             (t (format "%s 的云村动态" nickname)))
     :link (format "https://music.163.com/#/event?id=%s&uid=%s" id user-id)
     :date (/ (float (or (plist-get event :eventTime) 0)) 1000.0)
     :author nickname
     :body
     (concat
      (format "<p>%s</p>"
              (replace-regexp-in-string "\n" "<br>"
                                        (xml-escape-string message) t t))
      (nnneteasemusic--song-html song)
      (mapconcat
       (lambda (picture)
         (if-let* ((source (nnneteasemusic--https
                            (plist-get picture :originUrl))))
             (format "<p><img src=\"%s\"></p>"
                     (xml-escape-string source))
           ""))
       pictures "")))))

(defun nnneteasemusic--request-json (user-id)
  "Fetch public events for USER-ID as parsed JSON."
  (let ((url (format "https://music.163.com/api/event/get/%s" user-id))
        (url-request-extra-headers
         '(("Referer" . "https://music.163.com/"))))
    (with-current-buffer (or (url-retrieve-synchronously
                              url t t nnneteasemusic-request-timeout)
                             (error "Could not fetch %s" url))
      (unwind-protect
          (progn
            (goto-char (point-min))
            (unless (looking-at "HTTP/[0-9.]+ 2[0-9][0-9]")
              (error "NetEase Music API request failed: %s"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
            (unless (re-search-forward "\r?\n\r?\n" nil t)
              (error "NetEase Music API response has no body"))
            (json-parse-buffer :object-type 'plist :array-type 'list
                               :null-object nil :false-object nil))
        (kill-buffer (current-buffer))))))

(defun nnneteasemusic--fetch (record)
  "Fetch current public events for RECORD."
  (let* ((payload (nnneteasemusic--request-json
                   (plist-get record :user-id)))
         (events (plist-get payload :events))
         (fallback-name (plist-get record :user-id)))
    (unless (and (equal (plist-get payload :code) 200) (listp events))
      (error "NetEase Music API returned no event list"))
    (mapcar (lambda (event) (nnneteasemusic--normalize event fallback-name))
            events)))

(defun nnneteasemusic--refresh (db record)
  "Merge current events into RECORD, preserving article numbers."
  (let ((entries (plist-get record :entries))
        (high (plist-get record :high)))
    (dolist (item (nnneteasemusic--fetch record))
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
    (nnneteasemusic--save db)))

(defun nnneteasemusic--message-id (entry)
  "Stable Message-ID for ENTRY."
  (format "<%s@music.163.invalid>" (plist-get entry :guid)))

(defun nnneteasemusic--entry (record article)
  "Find ARTICLE by number or Message-ID in RECORD."
  (cl-find-if
   (lambda (entry)
     (if (integerp article)
         (= article (plist-get entry :number))
       (equal article (nnneteasemusic--message-id entry))))
   (plist-get record :entries)))

(defun nnneteasemusic--header (entry)
  "Construct a Gnus mail header for ENTRY."
  (make-full-mail-header
   (plist-get entry :number)
   (replace-regexp-in-string "[\r\n]+" " " (plist-get entry :title))
   (replace-regexp-in-string "[\r\n]+" " " (plist-get entry :author))
   (let ((system-time-locale "C"))
     (format-time-string "%a, %d %b %Y %T %z"
                         (seconds-to-time (plist-get entry :date)) t))
   (nnneteasemusic--message-id entry) "" 0 0 "" nil))

(deffoo nnneteasemusic-request-create-group (group &optional server _args)
  (let ((db (nnneteasemusic--select server)))
    (nnneteasemusic--group db group t)
    (nnneteasemusic--save db)
    t))
(deffoo nnneteasemusic-close-group (_group &optional _server) t)
(deffoo nnneteasemusic-asynchronous-p () nil)
(deffoo nnneteasemusic-request-post (&optional _server)
  (nnheader-report 'nnneteasemusic "NetEase Music events are read-only here"))

(deffoo nnneteasemusic-request-scan (&optional group server)
  (condition-case problem
      (let ((db (nnneteasemusic--select server)))
        (dolist (record (if group (list (nnneteasemusic--group db group))
                          (nnneteasemusic--db-groups db)))
          (when record (nnneteasemusic--refresh db record)))
        t)
    (error (nnheader-report 'nnneteasemusic "%s"
                            (error-message-string problem)))))

(deffoo nnneteasemusic-request-group (group &optional server _fast _info)
  (if-let* ((record (nnneteasemusic--group
                    (nnneteasemusic--select server) group)))
      (nnheader-insert "211 %d 1 %d %s\n" (length (plist-get record :entries))
                       (plist-get record :high) group t)
    (nnheader-report 'nnneteasemusic "Unknown NetEase Music subscription")))

(deffoo nnneteasemusic-request-list (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (record (nnneteasemusic--db-groups
                     (nnneteasemusic--select server)))
      (insert (format "%s %d 1 n\n" (plist-get record :name)
                      (plist-get record :high)))))
  t)
(deffoo nnneteasemusic-retrieve-groups (_groups &optional server)
  (nnneteasemusic-request-list server) 'active)
(deffoo nnneteasemusic-request-list-newsgroups (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (record (nnneteasemusic--db-groups
                     (nnneteasemusic--select server)))
      (insert (plist-get record :name) "\t云村动态 · "
              (plist-get record :user-id) "\n")))
  t)

(deffoo nnneteasemusic-retrieve-headers (articles &optional group server _fetch-old)
  (let ((record (nnneteasemusic--group
                 (nnneteasemusic--select server) group)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (number articles)
        (when-let* ((entry (and record (nnneteasemusic--entry record number))))
          (nnheader-insert-nov (nnneteasemusic--header entry))))))
  'nov)

(deffoo nnneteasemusic-request-article (article &optional group server buffer)
  (let* ((record (nnneteasemusic--group
                  (nnneteasemusic--select server) group))
         (entry (and record (nnneteasemusic--entry record article))))
    (if (not entry)
        (nnheader-report 'nnneteasemusic "Article is absent from the snapshot")
      (let ((header (nnneteasemusic--header entry)))
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
(defun nnneteasemusic-subscribe-user (user-id)
  "Subscribe to NetEase Music numeric USER-ID's events in Gnus."
  (interactive "sNetEase Music user ID: ")
  (unless (string-match-p "\\`[1-9][0-9]*\\'" user-id)
    (user-error "Enter a numeric NetEase Music user ID"))
  (unless (gnus-alive-p) (gnus-no-server))
  (let* ((server "music.163.com")
         (method `(nnneteasemusic ,server))
         (group (concat "events." user-id))
         (full (gnus-group-prefixed-name group method)))
    (unless (nnneteasemusic-open-server server)
      (error "%s" nnneteasemusic-status-string))
    (with-current-buffer gnus-group-buffer
      (unless (gnus-group-entry full) (gnus-group-make-group group method))
      (gnus-group-change-level full gnus-level-default-subscribed))
    (unless (nnneteasemusic-request-scan group server)
      (error "%s" nnneteasemusic-status-string))
    (with-current-buffer gnus-group-buffer
      (gnus-group-read-group t t full))))

(provide 'nnneteasemusic)
;;; nnneteasemusic.el ends here
