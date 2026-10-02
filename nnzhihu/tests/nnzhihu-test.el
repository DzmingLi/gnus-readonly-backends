;;; nnzhihu-test.el --- Tests for the Zhihu Gnus backend -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nnzhihu)

(defconst nnzhihu-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defun nnzhihu-test--fixture (name)
  "Parse JSON fixture NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name nnzhihu-test--directory))
    (json-parse-buffer :object-type 'plist :array-type 'list)))

(ert-deftest nnzhihu-supports-person-org-articles-and-person-answers ()
  (should (equal (nnzhihu--parts "articles.people.some-person")
                 '(articles "people" "some-person")))
  (should (equal (nnzhihu--parts "articles.org.some-org")
                 '(articles "org" "some-org")))
  (should (equal (nnzhihu--parts "answers.some-person")
                 '(answers "people" "some-person")))
  (should (equal (nnzhihu--parts "articles.people.marisa.moe")
                 '(articles "people" "marisa.moe")))
  (should-error (nnzhihu--parts "answers/bad-id")))

(ert-deftest nnzhihu-signs-requests-with-browser-cookie ()
  (let ((nnzhihu-cookie-function
         (lambda (url)
           (should (string-prefix-p "https://www.zhihu.com/api/v4/" url))
           '(("d_c0" . "device-id") ("z_c0" . "login-token")))))
    (cl-letf (((symbol-function 'zhihu--zse-request-headers)
               (lambda (url body dc0)
                 (should (string-match-p "/articles?" url))
                 (should-not body)
                 (should (equal dc0 "device-id"))
                 '(("x-zse-96" . "signature")))))
      (let* ((url (nnzhihu--api-url
                   '(:kind "articles" :user-id "someone")))
             (headers (nnzhihu--headers url "https://www.zhihu.com/people/someone/")))
        (should (equal (cdr (assoc "Cookie" headers))
                       "d_c0=device-id; z_c0=login-token"))
        (should (equal (cdr (assoc "x-zse-96" headers)) "signature"))))))

(ert-deftest nnzhihu-normalizes-article-and-answer-html ()
  (let* ((article (nnzhihu--normalize
                   (car (plist-get (nnzhihu-test--fixture "articles.json") :data))
                   "articles"))
         (answer (nnzhihu--normalize
                  (car (plist-get (nnzhihu-test--fixture "answers.json") :data))
                  "answers")))
    (should (equal (plist-get article :guid) "zhihu-article-401"))
    (should (equal (plist-get article :link) "https://zhuanlan.zhihu.com/p/401"))
    (should (string-match-p "完整正文" (plist-get article :body)))
    (should (string-match-p "src=\"https://img.example/zhihu.jpg\""
                            (plist-get article :body)))
    (should (equal (plist-get answer :guid) "zhihu-answer-502"))
    (should (equal (plist-get answer :title) "测试问题"))
    (should (equal (plist-get answer :link)
                   "https://www.zhihu.com/question/123/answer/502"))
    (should (string-match-p "src=\"https://img.example/answer.jpg\""
                            (plist-get answer :body)))))

(ert-deftest nnzhihu-refresh-keeps-numbers-when-api-order-changes ()
  (let* ((directory (make-temp-file "nnzhihu-test-" t))
         (file (expand-file-name "state.json" directory))
         (db (make-nnzhihu--db :file file))
         (group (nnzhihu--group db "answers.someone" t))
         (feed (list '(:guid "zhihu-answer-1" :title "One")
                     '(:guid "zhihu-answer-2" :title "Two"))))
    (unwind-protect
        (cl-letf (((symbol-function 'nnzhihu--fetch)
                   (lambda (_group) feed)))
          (nnzhihu--refresh db group)
          (setq feed (list '(:guid "zhihu-answer-2" :title "Revised")
                           '(:guid "zhihu-answer-3" :title "Three")))
          (nnzhihu--refresh db group)
          (let* ((saved (nnzhihu--load file))
                 (items (plist-get (car (nnzhihu--db-groups saved)) :entries)))
            (dolist (pair '(("zhihu-answer-1" . 1)
                            ("zhihu-answer-2" . 2)
                            ("zhihu-answer-3" . 3)))
              (should (= (plist-get (cl-find (car pair) items
                                             :key (lambda (item)
                                                    (plist-get item :guid))
                                             :test #'equal)
                                    :number)
                         (cdr pair))))
            (should (= (plist-get (car (nnzhihu--db-groups saved)) :high) 3))))
      (delete-directory directory t))))

(ert-deftest nnzhihu-gnus-group-scan-and-html-article ()
  (let* ((directory (make-temp-file "nnzhihu-gnus-test-" t))
         (nnzhihu-directory directory)
         (nnzhihu--databases (make-hash-table :test #'equal))
         (nntp-server-buffer (generate-new-buffer " *nnzhihu-test-article*"))
         (item (nnzhihu--normalize
                (car (plist-get (nnzhihu-test--fixture "articles.json") :data))
                "articles")))
    (unwind-protect
        (cl-letf (((symbol-function 'nnzhihu--fetch)
                   (lambda (_record) (list item))))
          (should (nnzhihu-open-server "test"))
          (should (nnzhihu-request-create-group "articles.people.someone" "test"))
          (should (nnzhihu-request-scan "articles.people.someone" "test"))
          (should (equal (nnzhihu-request-article
                          1 "articles.people.someone" "test" nntp-server-buffer)
                         '("articles.people.someone" . 1)))
          (with-current-buffer nntp-server-buffer
            (goto-char (point-min))
            (should (search-forward "Content-Type: text/html; charset=utf-8" nil t))
            (re-search-forward "\n\n")
            (let ((html (decode-coding-string
                         (base64-decode-string
                          (buffer-substring-no-properties (point) (point-max)))
                         'utf-8)))
              (should (string-match-p "完整正文" html))
              (should (string-match-p "src=\"https://img.example/zhihu.jpg\""
                                      html))))
          (nnzhihu-request-list "test")
          (with-current-buffer nntp-server-buffer
            (should (string-match-p "articles.people.someone 1 1 n"
                                    (buffer-string)))))
      (kill-buffer nntp-server-buffer)
      (delete-directory directory t))))

;;; nnzhihu-test.el ends here
