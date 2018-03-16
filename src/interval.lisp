(in-package :music)

(deftype quality-type ()
  "A type of quality of an interval."
  `(member perfect major minor augmented diminished))

(defmethod chromatic-offset ((quality (eql 'perfect)))
  "Return the chromatic offset of a perfect quality type."
  0)

(defmethod chromatic-offset ((quality (eql 'major)))
  "Return the chromatic offset of a major quality type."
  0)

(defmethod chromatic-offset ((quality (eql 'minor)))
  "Return the chromatic offset of a minor quality type."
  -1)

(defmethod chromatic-offset ((quality (eql 'augmented)))
  "Return the chromatic offset of a augmented quality type."
  1)

(defmethod chromatic-offset ((quality (eql 'diminished)))
  "Return the chromatic offset of a diminished quality type."
  -1)

(defun print-quality-type (quality stream)
  "Print a shorthand of a quality type to a stream."
  (princ (case quality
	   (perfect 'perf)
	   (major 'maj)
	   (minor 'min)
	   (augmented 'aug)
	   (diminished 'dim)
	   (otherwise (error "Invalid quality type!")))
	 stream))

(defclass quality ()
  ((quality-type
    :initarg :type
    :type quality-type
    :accessor quality-type)))

(defclass augmented-diminished-quality (quality)
  ((quality-type
    :initarg :type
    :type (member augmented diminished)
    :accessor quality-type)
   (multiple
    :initarg :multiple
    :initform 1
    :type (integer 1)
    :accessor multiple)))

(defun make-quality (quality-type &optional multiple)
  "Construct a quality instance."
  (if (and (typep quality-type '(member augmented diminished))
	   multiple)
      (make-instance 'augmented-diminished-quality
		     :type quality-type
		     :multiple multiple)
      (make-instance 'quality
		     :type quality-type)))

(defmethod chromatic-offset ((quality quality))
  "Return the chromatic offset of quality."
  (chromatic-offset (quality-type quality)))

(defmethod chromatic-offset ((quality augmented-diminished-quality))
  "Return the chromatic offset of an aumented or diminished quality."
  (* (call-next-method)
     (multiple quality)))

(defmethod print-object ((quality quality) stream)
  "Print the shorthand for a quality to a stream."
  (print-quality-type (quality-type quality) stream))

(defmethod print-object ((quality augmented-diminished-quality) stream)
  "Print the shorthand for an augmented or diminished quality to a stream."
  (when (> (multiple quality) 1)
    (princ (multiple quality) stream))
  (call-next-method))

(defclass interval ()
  ((diatonic-value
    :initarg :diatonic-value
    :initform 1
    :type (integer 1)
    :accessor diatonic-value)
   (quality
    :initarg :quality
    :initform (make-quality 'perfect)
    :type quality
    :accessor quality)))

(defmethod print-object ((interval interval) stream)
  "Print the shorthand for an interval to a stream."
  (princ (quality interval) stream)
  (princ (diatonic-value interval) stream))

(defmethod chromatic-value ((interval interval))
  "Return the chromatic value of an interval."
  (diatonic-to-chromatic-value (diatonic-value interval)))
