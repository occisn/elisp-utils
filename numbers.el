;;; -*- lexical-binding: t; -*-

;;; === to test: (ert '(tag elisp-utils))

;;; same purpose as the other one below, apparently
(defun my/number-to-string-with-comma-as-thousand-separator (num)
  "Return a string corresponding to number NUM formatted with thousand separators (commas).
For instance: 1234 --> '1,234'
v1 as of 2025-09-07"
  (let ((str (number-to-string num)))
    (while (string-match "\\(.*[0-9]\\)\\([0-9]\\{3\\}\\)" str)
      (setq str (replace-match "\\1,\\2" nil nil str)))
    str))

(ert-deftest test-number-to-string-with-comma-as-thousand-separator ()
  :tags '(elisp-utils)
  (should (string= "123,456" (my/number-to-string-with-comma-as-thousand-separator 123456))))

;;; same purpose as the one above, apparently
(defun my/add-number-grouping (number &optional separator)
  "Return a string corresponding to NUMBER, which each 3-digit group separated by SEPARATOR, by default a comma.

For instance: 123456 as a number--> 123,456 as a string
(v1, available in occisn/elisp-utils GitHub repository)"
  (let ((num (number-to-string number))
	(op (or separator ",")))
    (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
      (setq num (concat 
		 (match-string 1 num) op
		 (match-string 2 num))))
    num))

(ert-deftest test-number-grouping ()
  :tags '(elisp-utils)
  (should (string= "1,234,567" (my/add-number-grouping 1234567))))

(defun my/reverse-number (n &optional acc0)
  "Reverse the N, which is supposed to be an integer >= 0.
For instance: 123 --> 321.
ACC0 is an accumulator used during recursion.
(v1, available in occisn/elisp-utils GitHub repository)"
  (let ((acc (or acc0 0)))
    (if (= n 0)
	acc
      (let ((f (floor n 10))
	    (r (mod n 10)))
	(nox/reverse-number f (+ (* 10 acc) r))))))

(ert-deftest test-reverse-number ()
  :tags '(elisp-utils)
  (should (= 0 (my/reverse-number 0)))
  (should (= 1 (my/reverse-number 1)))
  (should (= 321 (my/reverse-number 123))))

;; usage in traditional Emacs Lisp without cl-lib
(when nil  
  (letrec (
           ;; 'reverse-number'
           ;; Reverse the N, which is supposed to be an integer >= 0.
           ;; For instance: 123 --> 321.
           ;; ACC0 is an accumulator used during recursion.
           ;; (v1, available in occisn/elisp-utils GitHub repository)
           (sub-reverse-number (lambda (n acc0)
                                 
                                 (let ((acc (or acc0 0)))
                                   (if (= n 0)
	                               acc
                                     (let ((f (floor n 10))
	                                   (r (mod n 10)))
	                               (funcall sub-reverse-number f (+ (* 10 acc) r)))))))
           (reverse-number (lambda (n)
                             (funcall sub-reverse-number n 0))))
    
    ;; do something
    ))

;;;; === end
