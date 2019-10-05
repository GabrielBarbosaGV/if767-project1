(require 'bruteforce (or (probe-file "bruteforce.fasl") (probe-file "bruteforce.lisp")))
(require 'kmp (or (probe-file "kmp.fasl") (probe-file "kmp.lisp")))
(require 'boyer-moore (or (probe-file "bm.fasl") (probe-file "bm.lisp")))
(require 'sellers (or (probe-file "sellers.fasl") (probe-file "sellers.lisp")))
(require 'ukkonnen (or (probe-file "ukkonnen.fasl") (probe-file "ukkonen.lisp")))

(defvar *needs-help* nil
  "If help should be printed")

(defvar *help-string*
  "\
Welcome to pmt!
Usage:
pmt [options] pattern textfile [textfile...]

Options:
  -e, --edit emax: Maximum edit distance for matching algorithm given by emax, defaults to zero.
  -p, --pattern patternfile: File containing patterns to be found, one per line.
  -a, --algorithm algorithm_name: Name of algorithm to be run, can be one of 

          knuth-morris-pratt, kmp
          boyer-moore, bm
          sellers
          ukkonen

  -c, --count: Show occurrence count instead of occurrences themselves.
  -h, --help: Prints this message.")

(defvar *edit-distance* 0
  "How much given pattern can be edited")

(defvar *pattern-file* nil
  "Path to pattern file")

(defvar *algorithm-name* nil
  "Name of algorithm to execute")

(defvar *count-only* nil
  "If only count of occurrences should be returned")

(defvar *positional-arguments-start* 1
  "Position in argument list where positional arguments start")

(defvar *unknown-options* nil
  "List of unknown passed options")

(defun process-opts ()
  (setf *positional-arguments-start* 1)
  (when (= (length *posix-argv*) 1) (setf *needs-help* t))
  (do ((arg (cdr *posix-argv*) (cdr arg))) ((or *needs-help* (null arg)))
    (let ((opt-str (car arg)))
      (when (eq (aref opt-str 0) #\-)
	  (process-option opt-str (cdr arg))))))

(defun process-option (opt-str list-cdr)
  (let ((option (remove-leading-dashes opt-str))
	(parameter (car list-cdr)))
    (configure-parameter option parameter)))

(defun remove-leading-dashes (opt-str)
  (do ((i 0 (1+ i)))
      ((not (eq #\- (aref opt-str i)))
       (subseq opt-str i))))

(defun configure-parameter (option parameter)
  "\
Switch function to determine what parameters to set according
to functions"
  (cond
    ((is-among option "help" "h")
     (setf *needs-help* t)
     (incf *positional-arguments-start*))
    ((is-among option "edit" "e")
     (setf *edit-distance* (parse-integer parameter))
     (incf *positional-arguments-start* 2))
    ((is-among option "pattern" "p")
     (setf *pattern-file* parameter)
     (incf *positional-arguments-start* 2))
    ((is-among option "algorithm" "a")
     (setf *algorithm-name* parameter)
     (incf *positional-arguments-start* 2))
    ((is-among option "count" "c")
     (setf *count-only* t)
     (incf *positional-arguments-start*))
    (t (push
	(concatenate 'string "Unknown option:" parameter)
	*unknown-options*))))

(defun main ()
  (process-opts)
  (if *needs-help*
      (print-help)
      (run-algorithm-for-files)))

(defun print-help ()
  (print *help-string*))

(defun run-algorithm-for-files ()
  (let ((file-paths (get-file-paths)))
    (do ((cur-file-path file-paths (cdr cur-file-path)))
	((null cur-file-path))
      (run-algorithm-for-file (car cur-file-path)))))

(defun get-file-paths ()
  (let ((start-position (if (not (null *pattern-file*))
			    *positional-arguments-start*
			    (1+ *positional-arguments-start*))))
    (do ((c (nthcdr start-position *posix-argv*) (cdr c))
	 (files nil))
	((null c) (nreverse files))
      (push (car c) files))))

(defun run-algorithm-for-file (file-path)
  (let* ((patterns (get-patterns))
	 (algorithms (get-pattern-to-implementation-hash-table patterns))
	 (occlists nil))
    (with-open-file (in file-path)
      (do ((l (read-line in nil) (read-line in nil))) ((null l))
	(do ((pattern patterns (cdr pattern)))
	    ((null pattern))
	  (push
	   (funcall (gethash (car pattern) algorithms)
		    l (car pattern))
	   occlists))
	(unless *count-only*
	  (format
	   t
	   "~{~{~a: ~a~^~%~}~%~}~a~%~%"
	   (mapcar #'list (reverse patterns) occlists)
	   l)))
      (when *count-only*
	(maphash
	 #'(lambda (key value) (format t "~a: ~a~%" key value))
	 (get-occurrence-count-for-pattern patterns occlists))))))

(defun get-patterns ()
  (if (null *pattern-file*) (list (elt *posix-argv* *positional-arguments-start*))
      (with-open-file (in *pattern-file*)
	(do ((l (read-line in nil) (read-line in nil))
	     (patterns nil))
	    ((null l) patterns)
	  (push l patterns)))))

(defun get-pattern-to-implementation-hash-table (patterns)
  (if (not (null *algorithm-name*))
      (get-pattern-to-implementation-hash-table-by-algorithm-name patterns)
      (get-pattern-to-implementation-hash-table-by-options patterns)))

(defun get-pattern-to-implementation-hash-table-by-algorithm-name (patterns)
  (let ((pattern-to-algorithm (make-hash-table :test 'equal)))
    (cond
      ((is-among *algorithm-name* "knuth-morris-pratt" "kmp")
       (set-same-hash-table-entries-for-list
	pattern-to-algorithm
	patterns
	#'(lambda (text pattern)
	    (knuth-morris-pratt text pattern))))
      ((is-among *algorithm-name* "boyer-moore" "bm")
       (set-same-hash-table-entries-for-list
	pattern-to-algorithm
	patterns
	#'(lambda (text pattern)
	    (boyer-moore text pattern))))
      ((is-among *algorithm-name* "sellers")
       (set-same-hash-table-entries-for-list
	pattern-to-algorithm
	patterns
	#'(lambda (text pattern)
	    (sellers text pattern *edit-distance*))))
      ((is-among *algorithm-name* "ukkonen")
       (set-hash-table-entries-for-pattern-ukkonen
	pattern-to-algorithm
	patterns)))))

(defun get-pattern-to-implementation-hash-table-by-options (patterns)
  (flet ((choose-by-name (algorithm-name)
	   (setf *algorithm-name* algorithm-name)
	   (get-pattern-to-implementation-hash-table-by-algorithm-name patterns)))
	 (if (not (null *edit-distance*))
	     (choose-by-name "ukkonen")
	     (choose-by-name "boyer-moore"))))
  

(defun is-among (value &rest values)
  "\
Tests for membership of value among values,
used for brevity"
  (member value values :test 'equal))

(defun set-same-hash-table-entries-for-list (hash-table keys value)
  (dolist (key keys hash-table)
    (setf (gethash key hash-table) value))) 

(defun set-hash-table-entries-for-patterns-ukkonen (hash-table patterns)
  (dolist (pattern patterns)
    (setf (gethash pattern hash-table)
	  (let ((scanner (ukkonnen-scanner pattern *edit-distance*)))
	    #'(lambda (text pattern) (funcall scanner text))))))

(defun get-occurrence-count-for-pattern (patterns occlists)
  (let ((pattern-to-count (make-hash-table :test 'equal)))
    (set-same-hash-table-entries-for-list
     pattern-to-count patterns 0)
    (do ((cur-occlists-cdr (copy-list occlists)))
	((null cur-occlists-cdr) pattern-to-count)
      (dolist (pattern patterns)
	(incf (gethash pattern pattern-to-count)
	      (length (car cur-occlists-cdr)))
	(pop cur-occlists-cdr)))))
