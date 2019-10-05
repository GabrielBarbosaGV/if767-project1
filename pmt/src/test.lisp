;;;; This file exists only for simple testing, actual benchmarking
;;;; is to be done in other ways
(require 'main (or (probe-file #p"main.fasl") (probe-file #p"main.lisp")))

(defvar *test-pattern-file* "patterns.txt")
(defvar *test-file* "main.lisp")

(defun clear-posix-argv ()
  (setf *posix-argv* '("")))

(defun insert-in-list (value list position)
  (if (not (plusp position))
      (push value list)
      (let ((cdr-before-value (nthcdr (1- position) list))
	    (cdr-after-value (nthcdr position list)))
	(insert-between-cdrs value
			     cdr-before-value
			     cdr-after-value))))

(defun insert-between-cdrs (value cdr-before-value cdr-after-value)
  (setf (cdr cdr-before-value)
	(push value cdr-after-value)))

(defun test-kmp ()
  (setf *posix-argv*
	`("" "-a" "knuth-morris-pratt" "-p" ,*test-pattern-file* ,*test-file*))
  (main))

(defun test-bm ()
  (setf *posix-argv*
	`("" "-a" "boyer-moore" "-p" ,*test-pattern-file* ,*test-file*))
  (main))

(defun test-sellers ()
  (setf *posix-argv*
	`("" "-a" "sellers" "-p" ,*test-pattern-file* ,*test-file*))
  (main))

(defun test-ukkonen ()
  (setf *posix-argv*
	`("" "-a" "sellers" "-p" ,*test-pattern-file* ,*test-file*))
  (main))

(defun test-help ()
  (setf *posix-argv*
	`("" "-h"))
  (main))

(defun test-count ()
  (setf *posix-argv*
	`("" "-a" "boyer-moore" "-p" ,*test-pattern-file* "-c" ,*test-file*))
  (main))
