(in-package :music)

(defun symbol-from-char (char &optional (eof-p t) eof-v)
  "Return a symbol from a string designator."
  (read-from-string (string char) eof-p eof-v))

(defun num-char-p (c)
  "Whether a character is a number character."
  (find c "0123456789+-"))

(defun not-num-char-p (c)
  "Whether a character is not a number character."
  (not (num-char-p c)))

(defun read-until (until stream &optional escape consume-final-char)
  "Read from stream until a character.
`until' can be either a single character or a string of possible characters or a function."
  (loop
     :with result = (make-array 0
				:element-type 'character
				:fill-pointer 0
				:adjustable t)
     :for c = (peek-char nil stream nil nil)
     :until (or (null c)
		(cond ((stringp until)
		       (position c until))
		      ((functionp until)
		       (funcall until c))
		      (t (char= c until))))
     :if (and escape (char= c escape))
     :do (progn
	   (read-char stream)
	   (vector-push-extend (read-char stream) result))
     :else :do (vector-push-extend (read-char stream) result)
     :finally (progn
		(when consume-final-char
		  (read-char stream))
		(return result))))

(defun read-until-not (until stream &optional escape consume-final-char)
  "Read from stream until reaching a character that does not match any char in `until'."
  (read-until (lambda (c)
		(cond ((stringp until)
		       (not (position c until)))
		      ((functionp until)
		       (not (funcall until c)))
		      (t (not (char= c until)))))
	      stream escape consume-final-char))

(defun diatonic-to-chromatic-value (value)
  "Convert a diatonic value to a chromatic value."
  (declare (type (integer 1) value))
  (let ((multiple (floor (1- value) 7)))
    (+ (* 12 multiple)
       (case (diatonic-class value)
	 (1 0)
	 (2 2)
	 (3 4)
	 (4 5)
	 (5 7)
	 (6 9)
	 (7 11)
	 (otherwise (error "Invalid value!"))))))

(defun add-diatonic-values (a b)
  "Add two diatonic values together."
  (declare (type (integer 1) a b))
  (+ -1 a b))

(defun subtract-diatonic-values (a b)
  "Return the difference between two diatonic values."
  (declare (type (integer 1) a b))
  (1+ (abs (- (1- a) (1- b)))))

(defun diatonic-mod (diatonic-value divisor)
  "Modulo a diatonic value."
  (declare (type (integer 1) diatonic-value divisor))
  (1+ (mod (1- diatonic-value) divisor)))

(defmethod diatonic-class ((diatonic-value integer))
  "Return the class of a diatonic value."
  (diatonic-mod diatonic-value 7))

(defun rotate-left (list)
  "Rotate a list to the left."
  (append (cdr list) (list (car list))))

(defun rotate-right (list)
  "Rotate a list to the right."
  (cons (car (last list))
	(subseq list 0 (1- (length list)))))

(defmacro fset (location value)
  "Perform `setf' but return the old value."
  `(let* ((old ,location))
     (setf ,location ,value)
     old))

(defmacro finc (location value)
  "Perform `incf' but return the old value."
  `(let* ((old ,location))
     (incf ,location ,value)
     old))

(defmacro fdec (location value)
  "Perform `decf' but return the old value."
  `(let* ((old ,location))
     (decf ,location ,value)
     old))

(defun reciprocal (value)
  "Return the reciprocal of a value."
  (/ 1 value))

(defun any-p (object objects)
  "Whether an object is any of a list of objects."
  (find-if (lambda (s)
	     (equalp object s))
	   objects))

(defmethod repeat ((list list) &optional (n 2))
  "Repeat a list `n' times."
  (case n
    (0 nil)
    (1 list)
    (otherwise (append list (repeat list (1- n))))))

(defun sort-val (val)
  "T is sorted as 1 and nil is sorted as 0."
  (cond ((eq val t) 1)
	((eq val nil) 0)
	(t val)))

(defun score-sort (list &rest functions)
  "Sort a list using a series of functions to score items."
  (if functions
      (apply #'append
	     (loop
		:with groupings = (gather (sort (score list (car functions))
						(lambda (a b)
						  (> (sort-val (cdr a))
						     (sort-val (cdr b)))))
					  (lambda (a b)
					    (eq (cdr a)
						(cdr b))))
		:for grouping :in groupings
		:collect (apply #'score-sort
				(cons (mapcar #'car grouping)
				      (cdr functions)))))
      list))

(defun score (list fn)
  "Give items a score using a function."
  (mapcar (lambda (item)
	    (cons item (funcall fn item)))
	  list))

(defun gather (list &optional (predicate #'equalp) buffer)
  "Group consecutive equal items in a list."
  (if list
      (if buffer
	  (if (funcall predicate (car list) (car buffer))
	      (gather (cdr list) predicate (cons (car list) buffer))
	      (cons buffer (gather (cdr list) predicate (list (car list)))))
	  (gather (cdr list) predicate (list (car list))))
      (list buffer)))


