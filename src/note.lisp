(in-package :music)

;; TODO:
;;   better note reading (use read-until, and support reading beyond 0-9 ints)
;;   read and print Double Sharps and Double Flat signs
;;   make interval above and below for pitch classes less hacky
;;   also read # and b as alternatives???  and is and es

(deftype musical-rest ()
  "Indicates a musical rest."
  `(member rest r))

(defun musical-rest-p (object)
  "Whether something is a musical rest."
  (typep object 'musical-rest))

(deftype letter-name ()
  "A letter name of a note."
  `(member a b c d e f g))

(deftype accidental ()
  "An accidental symbol."
  `(member ♯ 𝄪 ♭ 𝄫 ♮ |#| b is es))

(defun letter-name-p (object)
  "Whether an object is a letter name."
  (typep object 'letter-name))

(defun accidental-p (object)
  "Whether an object is an accidental symbol."
  (typep object 'accidental))

(defmethod letter-name ((diatonic-class (eql 1))) 'c)
(defmethod letter-name ((diatonic-class (eql 2))) 'd)
(defmethod letter-name ((diatonic-class (eql 3))) 'e)
(defmethod letter-name ((diatonic-class (eql 4))) 'f)
(defmethod letter-name ((diatonic-class (eql 5))) 'g)
(defmethod letter-name ((diatonic-class (eql 6))) 'a)
(defmethod letter-name ((diatonic-class (eql 7))) 'b)

(defmethod chromatic-value ((letter-name (eql 'c))) 0)
(defmethod chromatic-value ((letter-name (eql 'd))) 2)
(defmethod chromatic-value ((letter-name (eql 'e))) 4)
(defmethod chromatic-value ((letter-name (eql 'f))) 5)
(defmethod chromatic-value ((letter-name (eql 'g))) 7)
(defmethod chromatic-value ((letter-name (eql 'a))) 9)
(defmethod chromatic-value ((letter-name (eql 'b))) 11)

(defmethod diatonic-value ((letter-name (eql 'c))) 1)
(defmethod diatonic-value ((letter-name (eql 'd))) 2)
(defmethod diatonic-value ((letter-name (eql 'e))) 3)
(defmethod diatonic-value ((letter-name (eql 'f))) 4)
(defmethod diatonic-value ((letter-name (eql 'g))) 5)
(defmethod diatonic-value ((letter-name (eql 'a))) 6)
(defmethod diatonic-value ((letter-name (eql 'b))) 7)

(defmethod chromatic-offset ((accidental (eql '𝄪))) 2)
(defmethod chromatic-offset ((accidental (eql '♯))) 1)
(defmethod chromatic-offset ((accidental (eql '|#|))) 1)
(defmethod chromatic-offset ((accidental (eql 'is))) 1)
(defmethod chromatic-offset ((accidental (eql '♮))) 0)
(defmethod chromatic-offset ((accidental (eql '||))) 0)
(defmethod chromatic-offset ((accidental null)) 0)
(defmethod chromatic-offset ((accidental (eql '♭))) -1)
(defmethod chromatic-offset ((accidental (eql 'b))) -1)
(defmethod chromatic-offset ((accidental (eql 'es))) -1)
(defmethod chromatic-offset ((accidental (eql '𝄫))) -2)

(defmethod chromatic-offset ((string string))
  "Return the chromatic offset disignated by a string."
  (loop
     :with offset = 0
     :for c :across string
     :do (incf offset (chromatic-offset (symbol-from-char c)))
     :finally (return offset)))

(defun print-chromatic-offset (offset stream)
  "Format a readable representation of a chromatic offset to a stream."
  (format stream "~v@{~A~:*~}" (abs offset) (if (plusp offset) #\♯ #\♭)))

(defclass pitch-class ()
  ((letter-name
   :initarg :letter-name
   :initform 'c
   :type letter-name
   :accessor letter-name)
  (chromatic-offset
   :initarg :chromatic-offset
   :initform 0
   :type integer
   :accessor chromatic-offset)))

(defmethod pitch-class ((string string))
  "Return a pitch class designated by a string."
  (make-instance
   'pitch-class
   :letter-name (symbol-from-char (char string 0))
   :chromatic-offset (chromatic-offset (subseq string 1))))

(defmethod pitch-class ((symbol symbol))
  "Return a pitch class designated by a symbol."
  (pitch-class (string symbol)))

(defmethod pitch-class ((pitch-class pitch-class))
  "Return a pitch class designated by itself."
  pitch-class)

(defun make-pitch-class (diatonic-class chromatic-offset)
  "Construct a pitch class from a diatonic class and a chromatic offset."
  (make-instance
   'pitch-class
   :letter-name (letter-name diatonic-class)
   :chromatic-offset chromatic-offset))

(defmethod print-object ((pitch-class pitch-class) stream)
  "Print a readable representation of a pitch class to a stream."
  (princ (letter-name pitch-class) stream)
  (print-chromatic-offset (chromatic-offset pitch-class) stream))
  
(defmethod chromatic-value ((pitch-class pitch-class))
  "Return pitch the chromatic value of a pitch class."
  (+ (chromatic-value (letter-name pitch-class))
     (chromatic-offset pitch-class)))

(defmethod diatonic-value ((pitch-class pitch-class))
  "Return the diatonic value of a pitch class."
  (diatonic-value (letter-name pitch-class)))

(defclass note ()
  ((pitch-class
    :initarg :pitch-class
    :initform (make-instance 'pitch-class)
    :type pitch-class
    :accessor pitch-class)
   (octave
    :initarg :octave
    :initform 4
    :type (integer 0 9)
    :accessor octave)))

;; currently assuming octave numbers are from 0 to 9 and thus only one character long
(defmethod note ((string string))
  "Return a note designated by a string."
  (make-instance
   'note
   :pitch-class (pitch-class (subseq string 0 (1- (length string))))
   :octave (parse-integer (string (char string (1- (length string)))))))

(defmethod note ((symbol symbol))
  "Return a note designated by a symbol."
  (note (string symbol)))

(defmethod note ((note note))
  "Return a note designated by itself."
  note)

(defun make-note (diatonic-value chromatic-value)
  "Construct a note with a diatonic value and a chromatic value."
  (let* ((octave (1- (floor (1- diatonic-value) 7)))
	 (diatonic-class (subtract-diatonic-values
			  diatonic-value
			  (octave-diatonic-offset octave)))
	 (natural-chromatic-value (+ (diatonic-to-chromatic-value diatonic-class)
				     (octave-chromatic-offset octave)))
	 (chromatic-offset (- chromatic-value natural-chromatic-value))
	 (pitch-class (make-pitch-class diatonic-class chromatic-offset)))
    (make-instance
     'note
     :pitch-class pitch-class
     :octave octave)))

(defmethod letter-name ((note note))
  "Return the letter name of a note."
  (letter-name (pitch-class note)))

(defmethod chromatic-offset ((note note))
  "Return the chromatic value of a note."
  (chromatic-offset (pitch-class note)))

(defmethod chromatic-class ((note note))
  "Return the chromatic class of a note."
  (chromatic-value (pitch-class note)))

(defmethod diatonic-class ((note note))
  "Return the diatonic class of a note."
  (diatonic-value (pitch-class note)))

(defun octave-chromatic-offset (octave)
  "Return the chromatic offset of an octave."
  (* 12 (1+ octave)))

(defun octave-diatonic-offset (octave)
  "Return the diatonic offset of an octave."
  (1+ (* 7 (1+ octave))))

(defmethod chromatic-value ((note note))
  "Return the chromatic value of a note."
  (+ (chromatic-class note)
     (octave-chromatic-offset (octave note))))

(defmethod diatonic-value ((note note))
  "Return the diatonic value of a note."
  (add-diatonic-values (diatonic-class note)
		       (octave-diatonic-offset (octave note))))

(defmethod print-object ((note note) stream)
  "Print a readable representation of a note to a stream."
  (princ (pitch-class note) stream)
  (princ (octave note) stream))

(defmethod difference ((a note) (b note))
  "Return the interval between two notes."
  (make-interval (subtract-diatonic-values (diatonic-value a)
					   (diatonic-value b))
		 (abs (- (chromatic-value a)
			 (chromatic-value b)))))

(defmethod above ((note note) (interval interval))
  "Return the note an interval above another note."
  (make-note (add-diatonic-values (diatonic-value note)
				  (diatonic-value interval))
	     (+ (chromatic-value note)
		(chromatic-value interval))))

(defmethod below ((note note) (interval interval))
  "Return the note an interval below another note."
  (make-note (subtract-diatonic-values (diatonic-value note)
				       (diatonic-value interval))
	     (- (chromatic-value note)
		(chromatic-value interval))))

;; this is hacky...
(defmethod above ((pitch-class pitch-class) (interval interval))
  "Return the pitch class an interval above another pitch class."
  (pitch-class (above (make-instance 'note :pitch-class pitch-class)
		      interval)))

(defmethod below ((pitch-class pitch-class) (interval interval))
  "Return the pitch class an interval below another pitch class."
  (pitch-class (below (make-instance 'note :pitch-class pitch-class)
		      interval)))

(defmethod note-or-pitch-class ((string string))
  "Return either a note or a pitch class designated by a string."
  (if (num-char-p (char string (1- (length string))))
      (note string)
      (pitch-class string)))

(defmethod note-or-pitch-class ((symbol symbol))
  "Return either a note or a pitch class designated by a symbol."
  (note-or-pitch-class (string symbol)))

(defmethod note-or-pitch-class ((note note))
  "Return a note designated by itself."
  note)

(defmethod note-or-pitch-class ((pitch-class pitch-class))
  "Return a pitch class designated by itself."
  pitch-class)

(defmethod nearest ((pitch-class pitch-class) (note note))
  "Return the note with a pitch class nearest to a note."
  (let ((octave-offset
	 (if (> (subtract-diatonic-values
		 (diatonic-value pitch-class)
		 (diatonic-class note))
		4)
	     (if (> (diatonic-class note) 4)
		 1 -1)
	     0)))
    (make-instance
     'note
     :pitch-class pitch-class
     :octave (+ (octave note) octave-offset))))

(defmethod realize ((note note) &optional (env (default-environment)))
  "Realize a note from itself in a musical environment."
  (declare (ignore env))
  note)

(defmethod realize ((pitch-class pitch-class) &optional (env (default-environment)))
  "Realize a note from a pitch class in a musical environment."
  (nearest pitch-class (reference env)))

(defmethod realize ((degree integer) &optional (env (default-environment)))
  "Realize a note from a scale degree in a musical environment."
  (realize (scale-degree (key env) degree) env))

(defmethod realize ((degree symbol) &optional (env (default-environment)))
  "Realize a note from a symbol in a musical environment."
  (if (typep degree 'musical-rest)
      degree
      (realize (if (typep degree '(or solfège-syllable scale-degree roman-numeral))
		   (scale-degree (key env) degree)
		   (note-or-pitch-class degree))
	       env)))

(defmethod realize ((notes list) &optional (env (default-environment)))
  "Realize multiple notes in a musical environment."
  (flet ((special-p (symbol) (or (any-p symbol '(> <))
				 (typep symbol 'musical-rest))))
    (loop
       :with env = (clone env)
       :for note :in notes
       :for spec-p = (special-p note)
       :for realization = (if spec-p
			      (cond ((eql note '<)
				     (below (reference env) (interval 'p8)))
				    ((eql note '>)
				     (above (reference env) (interval 'p8)))
				    (t note))
			      (realize note env))
       :do (unless (typep note 'musical-rest) (setf (reference env) (reference realization)))
       :unless (and spec-p
		    (not (typep note 'musical-rest))) :collect realization)))
