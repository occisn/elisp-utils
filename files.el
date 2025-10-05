;;; -*- lexical-binding: t; -*-

;;; === to test: (ert '(tag elisp-utils))

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

(defun my/file-size-Mo (filename)
   "Return file size of FILENAME in Mo.
(v1, available in occisn/elisp-utils GitHub repository)"
   (round
    (/
     (file-attribute-size
      (file-attributes filename))
     1000000)))

(defun my/nb-of-elements-in-directory (folder)
   "Return number of elements in FOLDER, including sub-folders (no recursive investigation of subdirectories).
(v1, available in occisn/elisp-utils GitHub repository)"
   (- (length (directory-files folder)) 2))

(defun my/size-of-folder-in-Mo (folder)
   "Return the size of FOLDER.
Requires PowerShell on Windows.
May return 0 in case of problem encoutered by PowerShell.
(v1, available in occisn/elisp-utils GitHub repository)"
   (let* ((cmd1 (format "(Get-ChildItem '%s' -Recurse | Measure-Object -Property Length -Sum -ErrorAction Stop).Sum" folder))
	  (cmd2 (format "powershell.exe -Command \"%s\"" cmd1))
	  (res (shell-command-to-string cmd2)))
     (/ (string-to-number (string-trim res)) 1000000)))

;;;; === end
