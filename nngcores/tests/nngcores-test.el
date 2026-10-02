;;; nngcores-test.el --- Tests for the GCORES Gnus backend -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nngcores)

(defconst nngcores-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defun nngcores-test--fixture (name)
  "Parse JSON fixture NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name nngcores-test--directory))
    (json-parse-buffer :object-type 'plist :array-type 'list)))

(ert-deftest nngcores-renders-current-talk-content-and-cover ()
  (let* ((payload (nngcores-test--fixture "talks.json"))
         (entry (nngcores--normalize (car (plist-get payload :data))
                                   (plist-get payload :included))))
    (should (equal (plist-get entry :guid) "gcores-talks-201"))
    (should (equal (plist-get entry :link)
                   "https://www.gcores.com/talks/201"))
    (should (equal (plist-get entry :author) "机核用户"))
    (should (string-match-p "https://image.gcores.com/cover.jpg"
                            (plist-get entry :body)))
    (should (string-match-p "<p>导语</p><p>完整正文</p>"
                            (plist-get entry :body)))))

(ert-deftest nngcores-renders-current-draftjs-paragraphs-and-gallery ()
  (let* ((content
          (json-serialize
           '(:blocks [(:type "unstyled" :text "第一段")
                      (:type "atomic" :text " "
                       :entityRanges [(:key 0 :offset 0 :length 1)])]
             :entityMap (:0 (:type "GALLERY"
                             :data (:caption "图片说明"
                                    :images [(:path "gallery.jpg")]))))))
         (html (nngcores--parse-content content)))
    (should (string-match-p "<p>第一段</p>" html))
    (should (string-match-p
             "<figure><img src=\"https://image.gcores.com/gallery.jpg\"></figure>"
             html))
    (should (string-match-p "<p>图片说明</p>" html))))

(ert-deftest nngcores-keeps-numbers-across-feed-reordering ()
  (let* ((directory (make-temp-file "nngcores-test-" t))
         (file (expand-file-name "state.json" directory))
         (db (make-nngcores--db :file file))
         (group (nngcores--group db "talks.31418" t))
         (feed (list '(:guid "gcores-talks-1" :title "One")
                     '(:guid "gcores-talks-2" :title "Two"))))
    (unwind-protect
        (cl-letf (((symbol-function 'nngcores--fetch)
                   (lambda (_group) feed)))
          (nngcores--refresh db group)
          (setq feed (list '(:guid "gcores-talks-2" :title "Revised")
                           '(:guid "gcores-talks-3" :title "Three")))
          (nngcores--refresh db group)
          (let* ((saved (nngcores--load file))
                 (items (plist-get (car (nngcores--db-groups saved)) :entries)))
            (dolist (pair '(("gcores-talks-1" . 1)
                            ("gcores-talks-2" . 2)
                            ("gcores-talks-3" . 3)))
              (should (= (plist-get (cl-find (car pair) items
                                             :key (lambda (item)
                                                    (plist-get item :guid))
                                             :test #'equal)
                                    :number)
                         (cdr pair))))))
      (delete-directory directory t))))

(ert-deftest nngcores-gnus-group-scan-and-html-article ()
  (let* ((directory (make-temp-file "nngcores-gnus-test-" t))
         (nngcores-directory directory)
         (nngcores--databases (make-hash-table :test #'equal))
         (nntp-server-buffer (generate-new-buffer " *nngcores-test-article*"))
         (payload (nngcores-test--fixture "talks.json"))
         (entry (nngcores--normalize
                 (car (plist-get payload :data))
                 (plist-get payload :included))))
    (unwind-protect
        (cl-letf (((symbol-function 'nngcores--fetch)
                   (lambda (_record) (list entry))))
          (should (nngcores-open-server "test"))
          (should (nngcores-request-create-group "talks.31418" "test"))
          (should (nngcores-request-scan "talks.31418" "test"))
          (should (equal (nngcores-request-article
                          1 "talks.31418" "test" nntp-server-buffer)
                         '("talks.31418" . 1)))
          (with-current-buffer nntp-server-buffer
            (goto-char (point-min))
            (should (search-forward "Content-Type: text/html; charset=utf-8" nil t))
            (re-search-forward "\n\n")
            (let ((html (decode-coding-string
                         (base64-decode-string
                          (buffer-substring-no-properties (point) (point-max)))
                         'utf-8)))
              (should (string-match-p "完整正文" html))
              (should (string-match-p "image.gcores.com/cover.jpg" html))))
          (nngcores-request-list "test")
          (with-current-buffer nntp-server-buffer
            (should (string-match-p "talks.31418 1 1 n" (buffer-string)))))
      (kill-buffer nntp-server-buffer)
      (delete-directory directory t))))

;;; nngcores-test.el ends here
