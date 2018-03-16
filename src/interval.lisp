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
  (declare (type quality-type quality))
  (princ (case quality
	   (perfect 'p)
	   (major 'maj)
	   (minor 'min)
	   (augmented 'aug)
	   (diminished 'dim))
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

(defun perfect-quality-from-chromatic-offset (chromatic-offset)
  "Return a perfect, augmented, or diminished quality with a chromatic offset."
  (cond ((plusp chromatic-offset)
	 (make-quality 'augmented chromatic-offset))
	((zerop chromatic-offset)
	 (make-quality 'perfect))
	((minusp chromatic-offset)
	 (make-quality 'diminished (- chromatic-offset)))))

(defun major-minor-quality-from-chromatic-offset (chromatic-offset)
  "Return a major, minor, augmented, or diminished quality from a chromatic offset."
  (cond ((plusp chromatic-offset)
	 (make-quality 'augmented chromatic-offset))
	((zerop chromatic-offset)
	 (make-quality 'major chromatic-offset))
	((= -1 chromatic-offset)
	 (make-quality 'minor -1))
	((minusp chromatic-offset)
	 (make-quality 'diminished (1- (- chromatic-offset))))))

(defun quality-from-chromatic-offset (chromatic-offset diatonic-value)
  "Return a quality from a chromatic offset for a diatonic-value."
  (declare (type (integer 1) diatonic-value))
  (funcall
   (case (diatonic-class diatonic-value)
     ((1 4 5) #'perfect-quality-from-chromatic-offset)
     ((2 3 6 7) #'major-minor-quality-from-chromatic-offset))
   chromatic-offset))

(defmethod quality ((string string))
  "Return a quality designated by a string."
  (let* ((stream (make-string-input-stream string))
	 (multiple-str (read-until #'not-num-char-p stream))
	 (multiple (when (plusp (length multiple-str))
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
  (let* ((stream (make-string-input-stream string))
	 (multiple-str (read-until #'not-num-char-p stream))
	 (quality-type-str (read-until #'num-char-p stream))
	 (diatonic-value (read stream))
	 (quality (make-quality (quality-type (read-from-string quality-type-str nil))
				(read-from-string multiple-str nil)))
	 (diatonic-class (diatonic-class diatonic-value)))
    (case (quality-type quality)
      (perfect (assert (typep diatonic-class '(member 1 4 5))))
      ((major minor) (assert (typep diatonic-class '(member 2 3 6 7)))))
    (make-instance
     'interval
     :diatonic-value diatonic-value
     :quality quality)))

(defmethod interval ((symbol symbol))
  "Return an interval designated by a symbol."
  (interval (string symbol)))

(defmethod print-object ((interval interval) stream)
  "Print the shorthand for an interval to a stream."
  (princ (quality interval) stream)
  (princ (diatonic-value interval) stream))

(defmethod quality-type ((interval interval))
  "Return the quality type of an interval."
  (quality-type (quality interval)))

(defmethod diatonic-class ((interval interval))
  "Return the diatonic class of an interval."
  (diatonic-class (diatonic-value interval)))

(defmethod chromatic-offset ((interval interval))
  "Return the chromatic offset of the quality of an interval."
  (let ((chromatic-offset (chromatic-offset (quality interval))))
    (+ chromatic-offset
       (case (diatonic-class interval)
	 ((1 4 5) 0)
	 ((2 3 6 7)
	  (if (eql (quality-type interval)
		   'diminished)
	      -1 0))
	 (otherwise (error "Invalid interval!"))))))

(defmethod chromatic-value ((interval interval))
  "Return the chromatic value of an interval."
  (+ (diatonic-to-chromatic-value (diatonic-value interval))
     (chromatic-offset interval)))

(defmethod add ((a interval) (b interval))
  "Return the sum of two intervals."
  (let* ((diatonic-value (add-diatonic-values (diatonic-value a)
					      (diatonic-value b)))
	 (natural-chromatic-value (diatonic-to-chromatic-value diatonic-value))
	 (target-chromatic-value (+ (chromatic-value a)
				    (chromatic-value b)))
	 (chromatic-offset (- target-chromatic-value
			      natural-chromatic-value)))
    (make-instance
     'interval
     :diatonic-value diatonic-value
     :quality (quality-from-chromatic-offset chromatic-offset diatonic-value))))

