(in-package :music)

(defclass instrument ()
  ()
  (:documentation "Represents an instrument to play musical notes."))

(defclass adsr-instrument (instrument)
  ((adsr
    :initarg :asdr
    :initform (make-instance 'adsr)
    :type adsr
    :accessor adsr))
  (:documentation "An instrument with an ADSR envelope."))

(defclass envelope ()
  ()
  (:documentation "An envelope."))

(defgeneric on-value (envelope time)
  (:documentation "Get the value of an envelope at a given time since a trigger event."))

(defgeneric off-value (envelope time)
  (:documentation "Get the value of an envelope at a given time since an release event."))

(defclass adsr (envelope)
  ((attack
    :initarg :attack
    :initform 1/16
    :accessor attack
    :type (integer 0))
   (decay
    :initarg :decay
    :initform 1/2
    :accessor decay
    :type (integer 0))
   (sustain
    :initarg :sustain
    :initform 2/3
    :accessor sustain
    :type (integer 0 1))
   (release
    :initarg :release
    :initform 1/2
    :accessor release
    :type (integer 0)))
  (:documentation "An ADSR envelope."))

(defmethod on-value ((env adsr) time)
  "Get the value of an ADSR envelope at a given time since it was triggered."
  (cond ((< time (attack env))
	 (/ time (attack env)))
	((< time (+ (attack env)
		    (decay env)))
	 (- 1 (* (/ (- time (attack env))
		    (decay env))
		 (- 1 (sustain env)))))
	(t (sustain env))))

(defmethod off-value ((env adsr) time)
  "Get the value of an ADSR envelope at a given time since it was released."
  (max 0
       (- (sustain env)
	  (* (sustain env)
	     (/ time (release env))))))
