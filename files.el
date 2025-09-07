;;; -*- lexical-binding: t; -*-

;;; to test:
;;; (ert '(tag elisp-utils))

;;; === my/insert-directories-in-file-list

(defun my/insert-directories-in-file-list (files)
  "Take a list of files, and return the same list with directories intertwined.

For instance :
d1/a.org d1/b.org d2/c.org d3/d.org
-->
d1/ d1/a.org d1/b.org d2/ d2/c.org d3/ d3/d.org
(v1)"
  (let ((current-dir "")
	(files-intertwined-with-directories nil))
    (cl-loop for filename in files
	     for dir1 = (file-name-directory filename)
	     do (progn
		  (when (not (string= current-dir dir1))
		    (push dir1 files-intertwined-with-directories)
		    (setq current-dir dir1))
		  (push filename files-intertwined-with-directories)))
    (reverse files-intertwined-with-directories)))

(ert-deftest test-insert-directories-in-file-list ()
  :tags '(elisp-utils)
  (should (equal
	   '("d1/" "d1/a.org" "d1/b.org" "d2/" "d2/c.org" "d3/" "d3/d.org")
	   (my/insert-directories-in-file-list '("d1/a.org" "d1/b.org" "d2/c.org" "d3/d.org")))))

;;;; === end
