;;; nntheatlantic-test.el --- Tests for Atlantic Gnus backend -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nntheatlantic)

(defconst nntheatlantic-test--fixture
  (expand-file-name "author.xml" (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest nntheatlantic-parses-official-atom-and-renders-lead-image ()
  (let* ((xml (with-temp-buffer
                (insert-file-contents nntheatlantic-test--fixture)
                (buffer-string)))
         (entry (car (nntheatlantic--parse xml)))
         (html (nntheatlantic--render entry)))
    (should (equal (plist-get entry :guid) "tag:theatlantic.com,2026:50-687618"))
    (should (equal (plist-get entry :image) "https://cdn.theatlantic.com/lead.jpg"))
    (should (string-match-p "<figure><img src=" html))
    (should (string-match-p "First section" html))
    (should (string-match-p "<strong>Second section</strong>" html))))

(ert-deftest nntheatlantic-recirculation-option-affects-cached-article ()
  (let* ((body (concat "<p>Before</p>"
                       "<p class='more' data-id=\"injected-recirculation-link\"><i>"
                       "[<a href=\"https://www.theatlantic.com/category/wonder-reader/\">"
                       "The Wonder Reader</a>]</i></p>"
                       "<p>After</p>"))
         (entry (list :title "Story" :body body)))
    (let ((nntheatlantic-show-recirculation-links nil))
      (let ((html (nntheatlantic--render entry)))
        (should-not (string-match-p "Wonder Reader" html))
        (should-not (string-match-p "injected-recirculation-link" html))
        (should (string-match-p "<p>Before</p><p>After</p>" html))))
    (let ((nntheatlantic-show-recirculation-links t))
      (should (string-match-p "Wonder Reader" (nntheatlantic--render entry))))))

(ert-deftest nntheatlantic-preserves-numbers-across-feed-reordering ()
  (let* ((directory (make-temp-file "nntheatlantic-test-" t))
         (file (expand-file-name "state.json" directory))
         (db (make-nntheatlantic--db :file file))
         (record (nntheatlantic--group db "author.ian-bogost" t))
         (feed (list '(:guid "first" :title "First")
                     '(:guid "second" :title "Second"))))
    (unwind-protect
        (cl-letf (((symbol-function 'nntheatlantic--fetch)
                   (lambda (_slug) feed)))
          (nntheatlantic--refresh db record)
          (setq feed (list '(:guid "second" :title "Revised")
                           '(:guid "third" :title "Third")))
          (nntheatlantic--refresh db record)
          (let* ((saved (nntheatlantic--load file))
                 (items (plist-get (car (nntheatlantic--db-groups saved)) :entries)))
            (should (= (plist-get (cl-find "first" items :key
                                           (lambda (x) (plist-get x :guid)) :test #'equal)
                                  :number) 1))
            (should (= (plist-get (cl-find "second" items :key
                                           (lambda (x) (plist-get x :guid)) :test #'equal)
                                  :number) 2))
            (should (= (plist-get (cl-find "third" items :key
                                           (lambda (x) (plist-get x :guid)) :test #'equal)
                                  :number) 3))
            (should (= (plist-get (car (nntheatlantic--db-groups saved)) :high) 3))))
      (delete-directory directory t))))

;;; nntheatlantic-test.el ends here
