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
  (let ((multiple (floor (1- value) 7))
	(mod (mod (1- value) 7)))
    (+ (* 12 multiple)
       (case (1+ mod)
	 (1 0)
	 (2 2)
	 (3 4)
	 (4 5)
	 (5 7)
	 (6 9)
	 (7 11)
	 (otherwise (error "Invalid value!"))))))
