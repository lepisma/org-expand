;; Tests

(require 'ert-async)
(load-file "org-expand.el")

(ert-deftest-async test-youtube-url (done)
  (org-expand-get-youtube-url "End of Time Lacuna Coil"
                              (lambda (url)
                                (should (string-equal (second (s-split "=" url)) "USXHxgWoS9g"))
                                (funcall done))))

(ert-deftest-async test-wikipedia-summary (done)
  (org-expand-get-wikipedia-summary "dog"
                                    (lambda (summary)
                                      (should (> (length summary) 500)) ; I have faith
                                      (funcall done))))
