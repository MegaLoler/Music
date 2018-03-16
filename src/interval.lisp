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

(defmethod quality-type ((quality (eql 'perfect)))
  "Return a perfect quality type."
  'perfect)

(defmethod quality-type ((quality (eql 'perf)))
  "Return a perfect quality type."
  'perfect)

(defmethod quality-type ((quality (eql 'p)))
  "Return a perfect quality type."
  'perfect)

(defmethod quality-type ((quality (eql 'major)))
  "Return a major quality type."
  'major)

(defmethod quality-type ((quality (eql 'maj)))
  "Return a major quality type."
  'major)

(defmethod quality-type ((quality (eql 'ma)))
  "Return a major quality type."
  'major)

(defmethod quality-type ((quality (eql 'mj)))
  "Return a major quality type."
  'major)

(defmethod quality-type ((quality (eql 'minor)))
  "Return a minor quality type."
  'minor)

(defmethod quality-type ((quality (eql 'min)))
  "Return a minor quality type."
  'minor)

(defmethod quality-type ((quality (eql 'mi)))
  "Return a minor quality type."
  'minor)

(defmethod quality-type ((quality (eql 'mn)))
  "Return a minor quality type."
  'minor)

(defmethod quality-type ((quality (eql 'augmented)))
  "Return an augmented quality type."
  'augmented)

(defmethod quality-type ((quality (eql 'aug)))
  "Return an augmented quality type."
  'augmented)

(defmethod quality-type ((quality (eql 'a)))
  "Return an augmented quality type."
  'augmented)

(defmethod quality-type ((quality (eql 'diminished)))
  "Return an diminished quality type."
  'diminished)

(defmethod quality-type ((quality (eql 'dim)))
  "Return an diminished quality type."
  'diminished)

(defmethod quality-type ((quality (eql 'd)))
  "Return an diminished quality type."
  'diminished)

(defun print-quality-type (quality stream)
  "Print a shorthand of a quality type to a stream."
  (princ (case quality
	   (perfect 'p)
	   (major 'maj)
	   (minor 'min)
	   (augmented 'a)
	   (diminished 'd)
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

(defmethod quality ((string string))
  "Return a quality designated by a string."
  (let* ((stream (make-string-input-stream string))
	 (multiple-str (read-until #'not-num-char-p stream))
	 (multiple (when (length multiple-str)
		     (parse-integer multiple-str)))
	 (type-sym (read stream nil))
	 (type (quality-type type-sym)))
    (make-quality type multiple)))

(defmethod quality ((symbol symbol))
  "Return a quality designated by a symbol."
  (quality (string symbol)))

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

(defmethod interval ((string string))
  "Return an interval designated by a string."
  (make-instance
   'interval
   :diatonic-value (parse-integer (string (char string (1- (length string)))))
   :quality (quality (subseq string 0 (1- (length string))))))

(defmethod interval ((symbol symbol))
  "Return an interval designated by a symbol."
  (interval (string symbol)))

(defmethod print-object ((interval interval) stream)
  "Print the shorthand for an interval to a stream."
  (princ (quality interval) stream)
  (princ (diatonic-value interval) stream))

(defmethod chromatic-value ((interval interval))
  "Return the chromatic value of an interval."
  (diatonic-to-chromatic-value (diatonic-value interval)))
