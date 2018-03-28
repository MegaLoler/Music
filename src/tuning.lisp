(in-package :music)

(defclass tuning ()
  ((reference-note
    :accessor reference-note
    :initarg :reference-note
    :initform (note 'a4)
    :type note)
   (reference-frequency
    :accessor reference-frequency
    :initarg :reference-frequency
    :initform 440
    :type (integer 0)))
  (:documentation "A system of tuning musical notes."))

(defmethod reference-octave ((tuning tuning))
  "Return the reference octave of a tuning."
  (octave (reference-note tuning)))

(defgeneric tuning-frequency (tuning note)
  (:documentation "Return the frequency of a note according to a tuning system."))

(defclass equal-temperament (tuning)
  ())

(defmethod tuning-frequency ((tuning equal-temperament) note)
  "Return the frequency of a note according to equal temperament."
  (* (reference-frequency tuning)
     (expt (expt 2 1/12)
	   (- (chromatic-value note)
	      (chromatic-value (reference-note tuning))))))

(defvar *standard-tuning* (make-instance 'equal-temperament))
