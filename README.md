# elisp-utils

Personal utilities for Emacs Lisp.

Tests can be performed with `(ert '(tag elisp-utils))`

File **dates.el**  
   - function `my/lisp-timestamp-to-YYYY-MM-DD`  
   - function `my/YYYY-MM-DD-to-lisp-timestamp`  
   - function `my/today-YYYY-MM-DD`  
   - function `my/today-YYYY`  
   - function `my/today-MM`  
   - function `my/today-DD`  
   - function `my/day-in-week-in-French`  
   - function `my/date-to-day-in-week-in-French`  
   - function `my/today-day-in-week-in-French`  
   - function `my/day-number-in-French`  
   - function `my/month-in-French`  
   - function `my/english-month-to-number`  
   - function `my/today-in-French`

File **macros.el**  
   - macro `aprogn`  
   - macro `amapcar`  
   - macro `let+`  
   - macro `awhen`  
   - macro `aif`
   
File **numbers.el**  
   - function `my/number-to-string-with-comma-as-thousand-separator`  
   - function `my/add-number-grouping`  
   - function `my/reverse-number`  
   - function `my/isqrt--traditional`  
   - function `my/primep`  
   - function `my/primep-_traditional`  
   - function `my/gcd--traditional`  
   - function `my/lcm--traditional`  
   - function `my/largest-prime-factor`  
   - function `my/eratosthenes-sieve`  
   - function `my/eratosthenes-sieve--traditional`

File **files.el**  
   - function `my/insert-directories-in-file-list`  
   - function `my/get-file-last-modification-date`  
   - function `my/file-size-Mo`  
   - function `my/nb-of-elements-in-directory`  
   - function `my/size-of-folder-in-Mo`  
   - function `my/list-of-directories-and-subdirectories-from`
   
File **strings.el**  
   - function `my/string-remove-surrounding-quotes`  
   - function `my/string-suffix-p`  
   - function `my/split-string-at-first-delimiter`

File **windows.el**  
   - function `add-to-environment-variable`
   
(end of README)
