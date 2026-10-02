;;; nnblogger-test.el --- Tests for Blogger Gnus backend -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nnblogger)

(defconst nnblogger-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defun nnblogger-test--fixture (name)
  "Return fixture NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name nnblogger-test--directory))
    (buffer-string)))

(ert-deftest nnblogger-request-text-skips-http-headers ()
  (let ((body (nnblogger-test--fixture "feed.xml")))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (let ((buffer (generate-new-buffer " *nnblogger-response*")))
                   (with-current-buffer buffer
                     (set-buffer-multibyte nil)
                     (insert "HTTP/1.1 200 OK\r\n"
                             "Content-Type: application/atom+xml\r\n"
                             "Connection: close\r\n\r\n"
                             (encode-coding-string body 'utf-8)))
                   buffer))))
      (should (equal (nnblogger--request-text "https://example.test/feed")
                     body)))))

(ert-deftest nnblogger-parses-atom-and-full-post ()
  (let* ((entry (car (nnblogger--parse-feed
                      (nnblogger-test--fixture "feed.xml"))))
         (body (nnblogger--post-html
                (nnblogger-test--fixture "article.html"))))
    (should (equal (plist-get entry :guid) "tag:blogger.com,1999:blog-1.post-42"))
    (should (equal (plist-get entry :title) "定海水操"))
    (should (equal (plist-get entry :author) "Dostoe"))
    (should-not (plist-get entry :full))
    (should (string-match-p "被截断的摘要" (plist-get entry :body)))
    (should (string-match-p "<blockquote>水操尤奇在夜战" body))
    (should (string-match-p "img.example/blogger.jpg" body))))

(ert-deftest nnblogger-keeps-full-body-and-stable-numbers ()
  (let* ((directory (make-temp-file "nnblogger-test-" t))
         (file (expand-file-name "state.json" directory))
         (db (make-nnblogger--db :file file))
         (record (nnblogger--group db "posts.dostoe.blogspot.com" t))
         (feed (list '(:guid "post-1" :updated "same" :body "Summary"
                             :full nil))))
    (unwind-protect
        (cl-letf (((symbol-function 'nnblogger--fetch)
                   (lambda (_record) feed)))
          (nnblogger--refresh db record)
          (let ((first (car (plist-get record :entries))))
            (setf (plist-get first :body) "Full article"
                  (plist-get first :full) t))
          (setq feed (list '(:guid "post-2" :updated "new" :body "Second"
                                  :full nil)
                           '(:guid "post-1" :updated "same" :body "Summary"
                                  :full nil)))
          (nnblogger--refresh db record)
          (let* ((saved (nnblogger--load file))
                 (entries (plist-get (car (nnblogger--db-groups saved)) :entries))
                 (first (cl-find "post-1" entries
                                 :key (lambda (item) (plist-get item :guid))
                                 :test #'equal))
                 (second (cl-find "post-2" entries
                                  :key (lambda (item) (plist-get item :guid))
                                  :test #'equal)))
            (should (= (plist-get first :number) 1))
            (should (= (plist-get second :number) 2))
            (should (equal (plist-get first :body) "Full article"))
            (should (plist-get first :full))))
      (delete-directory directory t))))

(ert-deftest nnblogger-gnus-opens-full-html-article ()
  (let* ((directory (make-temp-file "nnblogger-gnus-test-" t))
         (nnblogger-directory directory)
         (nnblogger--databases (make-hash-table :test #'equal))
         (nntp-server-buffer (generate-new-buffer " *nnblogger-test*"))
         (entry (car (nnblogger--parse-feed
                      (nnblogger-test--fixture "feed.xml")))))
    (unwind-protect
        (cl-letf (((symbol-function 'nnblogger--fetch)
                   (lambda (_record) (list entry)))
                  ((symbol-function 'nnblogger--request-text)
                   (lambda (_url) (nnblogger-test--fixture "article.html"))))
          (should (nnblogger-open-server "test"))
          (should (nnblogger-request-create-group
                   "posts.dostoe.blogspot.com" "test"))
          (should (nnblogger-request-scan
                   "posts.dostoe.blogspot.com" "test"))
          (should (equal (nnblogger-request-article
                          1 "posts.dostoe.blogspot.com" "test"
                          nntp-server-buffer)
                         '("posts.dostoe.blogspot.com" . 1)))
          (with-current-buffer nntp-server-buffer
            (goto-char (point-min))
            (should (search-forward "Content-Type: text/html; charset=utf-8" nil t))
            (re-search-forward "\n\n")
            (let ((html (decode-coding-string
                         (base64-decode-string
                          (buffer-substring-no-properties (point) (point-max)))
                         'utf-8)))
              (should (string-match-p "完整正文" html))
              (should (string-match-p "img.example/blogger.jpg" html)))))
      (kill-buffer nntp-server-buffer)
      (delete-directory directory t))))

;;; nnblogger-test.el ends here
