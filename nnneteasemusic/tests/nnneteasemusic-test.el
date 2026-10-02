;;; nnneteasemusic-test.el --- Tests for NetEase Music Gnus backend -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nnneteasemusic)

(defconst nnneteasemusic-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defun nnneteasemusic-test--fixture ()
  "Parse the saved NetEase Music event payload."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "events.json" nnneteasemusic-test--directory))
    (json-parse-buffer :object-type 'plist :array-type 'list)))

(ert-deftest nnneteasemusic-normalizes-song-picture-and-play-link ()
  (let* ((event (car (plist-get (nnneteasemusic-test--fixture) :events)))
         (entry (nnneteasemusic--normalize event "fallback"))
         (body (plist-get entry :body)))
    (should (equal (plist-get entry :guid) "netease-event-301"))
    (should (equal (plist-get entry :title) "分享单曲 · 测试歌曲"))
    (should (equal (plist-get entry :author) "音乐用户"))
    (should (equal (plist-get entry :link)
                   "https://music.163.com/#/event?id=301&uid=9"))
    (should (string-match-p "云村正文" body))
    (should (string-match-p "测试音乐人" body))
    (should (string-match-p "测试专辑" body))
    (should (string-search "outchain/player?type=2&amp;id=88" body))
    (should (string-match-p "▶ 在网易云音乐播放" body))
    (should (string-match-p "https://img.example/music.jpg" body))
    (should-not (string-match-p "http://img.example/music.jpg" body))))

(ert-deftest nnneteasemusic-does-not-repeat-song-name-in-title ()
  (let* ((event (car (plist-get (nnneteasemusic-test--fixture) :events)))
         (thread (plist-get (plist-get event :info) :commentThread)))
    (setf (plist-get thread :resourceTitle) "分享单曲：「测试歌曲」")
    (should (equal (plist-get (nnneteasemusic--normalize event "fallback") :title)
                   "分享单曲：「测试歌曲」"))))

(ert-deftest nnneteasemusic-keeps-numbers-across-feed-reordering ()
  (let* ((directory (make-temp-file "nnneteasemusic-test-" t))
         (file (expand-file-name "state.json" directory))
         (db (make-nnneteasemusic--db :file file))
         (group (nnneteasemusic--group db "events.398686067" t))
         (feed (list '(:guid "netease-event-1" :title "One")
                     '(:guid "netease-event-2" :title "Two"))))
    (unwind-protect
        (cl-letf (((symbol-function 'nnneteasemusic--fetch)
                   (lambda (_group) feed)))
          (nnneteasemusic--refresh db group)
          (setq feed (list '(:guid "netease-event-2" :title "Revised")
                           '(:guid "netease-event-3" :title "Three")))
          (nnneteasemusic--refresh db group)
          (let* ((saved (nnneteasemusic--load file))
                 (items (plist-get
                         (car (nnneteasemusic--db-groups saved)) :entries)))
            (dolist (pair '(("netease-event-1" . 1)
                            ("netease-event-2" . 2)
                            ("netease-event-3" . 3)))
              (should (= (plist-get
                          (cl-find (car pair) items
                                   :key (lambda (item) (plist-get item :guid))
                                   :test #'equal)
                          :number)
                         (cdr pair))))))
      (delete-directory directory t))))

(ert-deftest nnneteasemusic-gnus-scan-and-html-article ()
  (let* ((directory (make-temp-file "nnneteasemusic-gnus-test-" t))
         (nnneteasemusic-directory directory)
         (nnneteasemusic--databases (make-hash-table :test #'equal))
         (nntp-server-buffer
          (generate-new-buffer " *nnneteasemusic-test-article*"))
         (event (car (plist-get (nnneteasemusic-test--fixture) :events)))
         (entry (nnneteasemusic--normalize event "fallback")))
    (unwind-protect
        (cl-letf (((symbol-function 'nnneteasemusic--fetch)
                   (lambda (_record) (list entry))))
          (should (nnneteasemusic-open-server "test"))
          (should (nnneteasemusic-request-create-group
                   "events.398686067" "test"))
          (should (nnneteasemusic-request-scan "events.398686067" "test"))
          (should (equal (nnneteasemusic-request-article
                          1 "events.398686067" "test" nntp-server-buffer)
                         '("events.398686067" . 1)))
          (with-current-buffer nntp-server-buffer
            (goto-char (point-min))
            (should (search-forward "Archived-at: <https://music.163.com/#/event?id=301&uid=9>"
                                    nil t))
            (should (search-forward "Content-Type: text/html; charset=utf-8"
                                    nil t))
            (re-search-forward "\n\n")
            (let ((html (decode-coding-string
                         (base64-decode-string
                          (buffer-substring-no-properties (point) (point-max)))
                         'utf-8)))
              (should (string-match-p "▶ 在网易云音乐播放" html))))
          (nnneteasemusic-request-list "test")
          (with-current-buffer nntp-server-buffer
            (should (string-match-p "events.398686067 1 1 n"
                                    (buffer-string)))))
      (kill-buffer nntp-server-buffer)
      (delete-directory directory t))))

;;; nnneteasemusic-test.el ends here
