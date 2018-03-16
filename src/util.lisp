(in-package :music)

(defun symbol-from-char (char)
  "Return a symbol from a character."
  (read-from-string (string char)))

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
