
;;; to test:
;;; (ert '(tag elisp-utils))

;;; === my/number-to-string-with-comma-as-thousand-separator

(defun my/number-to-string-with-comma-as-thousand-separator (num)
  "Return a string corresponding to number NUM formatted with thousand separators (commas).
For instance: 1234 --> '1,234'"
  (let ((str (number-to-string num)))
    (while (string-match "\\(.*[0-9]\\)\\([0-9]\\{3\\}\\)" str)
      (setq str (replace-match "\\1,\\2" nil nil str)))
    str))

(ert-deftest my/number-to-string-with-comma-as-thousand-separator ()
   :tags '(elisp-utils)
   (should (string= "123,456" (my/number-to-string-with-comma-as-thousand-separator 123456))))

;;;; === end
