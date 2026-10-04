;;; nnjanestreet-test.el --- Jane Street tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nnjanestreet)

(ert-deftest nnjanestreet-extracts-full-post-and-resolves-media ()
  (let* ((html "<aside>Navigation</aside><article><div class='post-header'><div class='featimg-wrapper'><img src='/hero.png'></div><div class='author img'><div class='name'>By: <a href='/author/a'>Ada Example</a></div></div></div><div class='post-content'><p>Full body after the excerpt</p><pre><code>a &lt; b</code></pre><img src='picture.png'><a href='/other/'>Link</a><script>tracking()</script></div></article><aside>Recommended posts</aside>")
         (post (nnjanestreet--post html "https://blog.janestreet.com/example/"))
         (body (plist-get post :body)))
    (should (equal (plist-get post :author) "Ada Example"))
    (should (string-match-p "Full body after the excerpt" body))
    (should (string-match-p "<pre><code>a &lt; b</code></pre>" body))
    (should (string-match-p "https://blog.janestreet.com/example/picture.png" body))
    (should (string-match-p "https://blog.janestreet.com/hero.png" body))
    (should-not (string-match-p "Navigation\\|Recommended\\|tracking" body))
    (should-error (nnjanestreet--post "<p>Unavailable</p>" "https://blog.janestreet.com/"))))

(ert-deftest nnjanestreet-index-refresh-keeps-cached-body-author-and-number ()
  (let* ((directory (make-temp-file "nnjanestreet-test-" t))
         (db (make-nnjanestreet--db :file (expand-file-name "state.json" directory)))
         (record (nnjanestreet--group db "posts" t))
         (feed (nnjanestreet--parse-feed
                "<rss><channel><item><title>Title</title><link>https://blog.janestreet.com/example/</link><pubDate>Thu, 05 Feb 2026 00:00:00 +0000</pubDate><description>&lt;p&gt;Excerpt&lt;/p&gt;</description></item></channel></rss>")))
    (unwind-protect
        (cl-letf (((symbol-function 'nnjanestreet--fetch)
                   (lambda (_) (copy-tree feed))))
          (nnjanestreet--refresh db record)
          (let ((entry (car (plist-get record :entries))))
            (setf (plist-get entry :body) "<p>Full text</p>"
                  (plist-get entry :author) "Ada Example"
                  (plist-get entry :full) t))
          (nnjanestreet--refresh db record)
          (let* ((saved (nnjanestreet--load (nnjanestreet--db-file db)))
                 (entry (car (plist-get (car (nnjanestreet--db-groups saved)) :entries))))
            (should (= (plist-get entry :number) 1))
            (should (plist-get entry :full))
            (should (equal (plist-get entry :author) "Ada Example"))
            (should (equal (plist-get entry :body) "<p>Full text</p>"))))
      (delete-directory directory t))))

(ert-deftest nnjanestreet-failed-full-fetch-remains-retryable ()
  (let ((entry (list :link "https://blog.janestreet.com/example/"
                     :body "Excerpt" :author "Jane Street" :full nil)))
    (cl-letf (((symbol-function 'nnjanestreet--request-text)
               (lambda (_) (error "Network unavailable"))))
      (should-error (nnjanestreet--ensure-full-body nil entry))
      (should-not (plist-get entry :full))
      (should (equal (plist-get entry :body) "Excerpt")))))
