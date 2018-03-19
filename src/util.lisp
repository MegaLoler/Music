(in-package :music)

(defun symbol-from-char (char)
  "Return a symbol from a character."
  (read-from-string (string char)))

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
     :for c = (peek-char nil stream)
     :until (cond ((stringp until)
		   (position c until))
		  ((functionp until)
		   (funcall until c))
		  (t (char= c until)))
     :if (and escape (char= c escape))
     :do (progn
	   (read-char stream)
	   (vector-push-extend (read-char stream) result))
     :else :do (vector-push-extend (read-char stream) result)
     :finally (progn
		(when consume-final-char
		  (read-char stream))
		(return result))))

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
