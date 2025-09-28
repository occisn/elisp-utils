;;; -*- lexical-binding: t; -*-

;;; === to test: (ert '(tag elisp-utils))

(defun my/string-remove-surrounding-quotes (s)
  "Remove quotes at the beginning and at the end of a string.
(v1, available in occisn/elisp-utils GitHub repository)"
  (aprogn
   s
   (string-remove-prefix "\"" it)
   (string-remove-suffix "\"" it)))

(ert-deftest test-string-remove-surrounding-quotes ()
  :tags '(elisp-utils)
  (should (string= "abcdef" (my/string-remove-surrounding-quotes "\"abcdef\""))))

(defun my/string-suffix-p (suffix str &optional ignore-case)
  "Return tt if STR finished by SUFFIX.
Ignore case.
(v1, available in occisn/elisp-utils GitHub repository)
Source: https://stackoverflow.com/questions/22403751/check-if-a-string-ends-with-a-suffix-in-emacs-lisp" 
  (let ((begin2 (- (length str) (length suffix)))
        (end2 (length str)))
    (when (< begin2 0) (setq begin2 0))
    (eq t (compare-strings suffix nil nil
                           str begin2 end2
                           ignore-case))))

(ert-deftest test-string-suffix ()
  :tags '(elisp-utils)
  (should (my/string-suffix-p "def" "abcdef"))
  (should (my/string-suffix-p "DEF" "abcdef" t))
  (should (my/string-suffix-p "def" "abcDEF" t))
  (should (not (my/string-suffix-p "def" "abcdefg"))))

(defun my/split-string-at-first-delimiter (s)
  "Split string S at the first occurrence of either a space or a line return.
For instance: 'aa bb cc' --> ('aa' 'bb cc')
(v1, available in occisn/elisp-utils GitHub repository)"
  (if (string-match "\\( \\|\n\\)" s)
      (list (substring s 0 (match-beginning 0))
            (substring s (match-end 0)))
    (list s)))

(ert-deftest test-split-string-at-first-delimiter ()
  :tags '(elisp-utils)
  (should (equal '("aa" "bb cc") (my/split-string-at-first-delimiter "aa bb cc")))
  (should (equal '("aa" "bb cc") (my/split-string-at-first-delimiter "aa\nbb cc"))))

;;; end
