;;; nnpublicinbox-test.el --- Tests for public-inbox backend -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nnpublicinbox)

(defconst nnpublicinbox-test--mbox
  (concat
   "From mboxrd@z Thu Jan  1 00:00:00 1970\n"
   "From: A <a@example.org>\nSubject: A topic\n"
   "Date: Fri, 01 Mar 2024 20:34:35 +0000\n"
   "Message-ID: <root@example.org>\n"
   "Content-Type: text/plain; charset=utf-8\n\nHello\n>From a quote\n"
   "From mboxrd@z Thu Jan  1 00:00:00 1970\n"
   "From: B <b@example.org>\nSubject: Re: A topic\n"
   "Date: Sat, 02 Mar 2024 09:29:06 +0100\n"
   "Message-ID: <reply@example.org>\nReferences: <root@example.org>\n"
   "In-Reply-To: <root@example.org>\n\nReply\n"))

(ert-deftest nnpublicinbox-preserves-mail-thread-and-numbers ()
  (let* ((directory (make-temp-file "nnpublicinbox-test-" t))
         (nnpublicinbox-directory directory)
         (nnpublicinbox--databases (make-hash-table :test #'equal))
         (nntp-server-buffer (generate-new-buffer " *nnpublicinbox-test*"))
         (server "https://list.example.org")
         (group "list/root@example.org")
         (mbox nnpublicinbox-test--mbox))
    (unwind-protect
        (cl-letf (((symbol-function 'nnpublicinbox--fetch)
                   (lambda (_server _group) mbox)))
          (should (nnpublicinbox-open-server server))
          (should (nnpublicinbox-request-create-group group server))
          (should (nnpublicinbox-request-scan group server))
          (let* ((db nnpublicinbox--state)
                 (record (nnpublicinbox--group db group))
                 (entries (plist-get record :entries))
                 (reply (nnpublicinbox--entry record "<reply@example.org>")))
            (should (= (length entries) 2))
            (should (= (plist-get reply :number) 2))
            (should (equal (plist-get reply :references)
                           "<root@example.org>"))
            (should (string-match-p "\nFrom a quote\n"
                                    (plist-get (nnpublicinbox--entry record 1)
                                               :raw)))
            (nnpublicinbox-retrieve-headers '(1 2) group server)
            (with-current-buffer nntp-server-buffer
              (should (string-match-p
                       "<reply@example.org>\t<root@example.org>"
                       (buffer-string))))
            (should (equal (nnpublicinbox-request-article
                            2 group server nntp-server-buffer)
                           (cons group 2)))
            (setq mbox (concat nnpublicinbox-test--mbox
                               "From mboxrd@z Thu Jan  1 00:00:00 1970\n"
                               "From: C <c@example.org>\nSubject: Re: A topic\n"
                               "Date: Sun, 03 Mar 2024 09:00:00 +0100\n"
                               "Message-ID: <third@example.org>\n"
                               "References: <root@example.org> "
                               "<reply@example.org>\n\nThird\n"))
            (should (nnpublicinbox-request-scan group server))
            (should (= (plist-get (nnpublicinbox--entry record
                                                        "<reply@example.org>")
                                  :number) 2))
            (should (= (plist-get (nnpublicinbox--entry record
                                                        "<third@example.org>")
                                  :number) 3))
            (should (= (plist-get (nnpublicinbox--group
                                   (nnpublicinbox--load
                                    (nnpublicinbox--db-file db)) group)
                                  :high) 3))))
      (kill-buffer nntp-server-buffer)
      (delete-directory directory t))))

(ert-deftest nnpublicinbox-rejects-invalid-message-id ()
  (should-error (nnpublicinbox--parse-mbox
                 "From mboxrd@z Thu Jan  1 00:00:00 1970\nSubject: X\n\nX\n")))

;;; nnpublicinbox-test.el ends here
