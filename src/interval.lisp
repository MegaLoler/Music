(in-package :music)

(deftype quality-type ()
  "A type of quality of an interval."
  `(member perfect major minor augmented diminished))

(defmethod chromatic-offset ((quality (eql 'perfect))) 0)
(defmethod chromatic-offset ((quality (eql 'major))) 0)
(defmethod chromatic-offset ((quality (eql 'minor))) -1)
(defmethod chromatic-offset ((quality (eql 'augmented))) 1)
(defmethod chromatic-offset ((quality (eql 'diminished))) -1)

(defmethod quality-type ((quality (eql 'perfect))) 'perfect)
(defmethod quality-type ((quality (eql 'perf))) 'perfect)
(defmethod quality-type ((quality (eql 'p))) 'perfect)
(defmethod quality-type ((quality (eql 'major))) 'major)
(defmethod quality-type ((quality (eql 'maj))) 'major)
(defmethod quality-type ((quality (eql 'ma))) 'major)
(defmethod quality-type ((quality (eql 'mj))) 'major)
(defmethod quality-type ((quality (eql 'minor))) 'minor)
(defmethod quality-type ((quality (eql 'min))) 'minor)
(defmethod quality-type ((quality (eql 'mi))) 'minor)
(defmethod quality-type ((quality (eql 'mn))) 'minor)
(defmethod quality-type ((quality (eql 'augmented))) 'augmented)
(defmethod quality-type ((quality (eql 'aug))) 'augmented)
(defmethod quality-type ((quality (eql 'a))) 'augmented)
(defmethod quality-type ((quality (eql 'diminished))) 'diminished)
(defmethod quality-type ((quality (eql 'dim))) 'diminished)
(defmethod quality-type ((quality (eql 'd))) 'diminished)

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

(defmethod interval ((interval interval))
  "Return an interval designated by itself."
  interval)

(defun make-interval (diatonic-value chromatic-value)
  "Construct an interval from a diatonic value and a chromatic value."
  (let* ((natural-chromatic-value (diatonic-to-chromatic-value diatonic-value))
	 (chromatic-offset (- chromatic-value natural-chromatic-value)))
    (make-instance
     'interval
     :diatonic-value diatonic-value
     :quality (quality-from-chromatic-offset chromatic-offset diatonic-value))))

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

(defmethod operate
    ((a interval)
     (b interval)
     (diatonic-func function)
     (chromatic-func function))
  "Return the interval resulting from performing an operation on the diatonic and chromatic values of two intervals."
  (make-interval (funcall diatonic-func
			  (diatonic-value a)
			  (diatonic-value b))
		 (funcall chromatic-func
			  (chromatic-value a)
			  (chromatic-value b))))

(defmethod sum ((a interval) (b interval))
  "Return the sum of two intervals."
  (operate a b #'add-diatonic-values #'+))

(defmethod difference ((a interval) (b interval))
  "Return the difference between two intervals."
  (operate a b #'subtract-diatonic-values #'-))
