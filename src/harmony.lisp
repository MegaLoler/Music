(in-package :music)

;; TODO
;;   harmony constructor from roman numeral and chord notation
;;   give make-harmony ability to Alter members of the harmony!
;;   make build-harmony into a fancy macro that can take notes and/or intervals arbitrarily and stack them

(defun make-harmony (&optional (key (key 'c-major)) (degree 1) (inversion 0) (members 3) (interval 3))
  "Return a set of pitch classes of a harmony built from the scale of a key."
  (invert (loop
	     :for degree :from degree :by (1- interval)
	     :for mod = (diatonic-class degree)
	     :repeat members
	     :collect (scale-degree (key key) mod))
	  inversion))

(defun build-harmony (root &rest intervals)
  "Return a set of pitch classes or notes of a harmony given a root and a set of intervals."
  (let ((root (note-or-pitch-class root)))
    (mapcar (lambda (interval)
	      (above root (interval interval)))
	    (cons (interval 'p1) intervals))))

(defun invert (harmony &optional (count 1))
  "Invert a harmony."
  (cond ((plusp count)
	 (invert (rotate-left harmony) (1- count)))
	((minusp count)
	 (invert (rotate-right harmony) (1+ count)))
	(t harmony)))
