(in-package :music)

(deftype letter-name ()
  "A letter name of a note."
  `(member a b c d e f g))

(deftype accidental ()
  "An accidental symbol."
  `(member ♯ 𝄪 ♭ ♮))

(defun letter-name-p (object)
  "Whether an object is a letter name."
  (typep object 'letter-name))

(defun accidental-p (object)
  "Whether an object is an accidental symbol."
  (typep object 'accidental))

(defmethod chromatic-value ((letter-name (eql 'c)))
  "Return the chromatic value of the letter name C."
  0)

(defmethod chromatic-value ((letter-name (eql 'd)))
  "Return the chromatic value of the letter name D."
  2)

(defmethod chromatic-value ((letter-name (eql 'e)))
  "Return the chromatic value of the letter name E."
  4)

(defmethod chromatic-value ((letter-name (eql 'f)))
  "Return the chromatic value of the letter name F."
  5)

(defmethod chromatic-value ((letter-name (eql 'g)))
  "Return the chromatic value of the letter name G."
  7)

(defmethod chromatic-value ((letter-name (eql 'a)))
  "Return the chromatic value of the letter name A."
  9)

(defmethod chromatic-value ((letter-name (eql 'b)))
  "Return the chromatic value of the letter name B."
  11)

(defmethod diatonic-value ((letter-name (eql 'c)))
  "Return the diatonic value of the letter name C."
  1)

(defmethod diatonic-value ((letter-name (eql 'd)))
  "Return the diatonic value of the letter name D."
  2)

(defmethod diatonic-value ((letter-name (eql 'e)))
  "Return the diatonic value of the letter name E."
  3)

(defmethod diatonic-value ((letter-name (eql 'f)))
  "Return the diatonic value of the letter name F."
  4)

(defmethod diatonic-value ((letter-name (eql 'g)))
  "Return the diatonic value of the letter name G."
  5)

(defmethod diatonic-value ((letter-name (eql 'a)))
  "Return the diatonic value of the letter name A."
  6)

(defmethod diatonic-value ((letter-name (eql 'b)))
  "Return the diatonic value of the letter name B."
  7)

(defmethod chromatic-offset ((accidental (eql '𝄪)))
  "Return the chromatic value of a sharp accidental."
  2)

(defmethod chromatic-offset ((accidental (eql '♯)))
  "Return the chromatic value of a sharp accidental."
  1)

(defmethod chromatic-offset ((accidental (eql '♭)))
  "Return the chromatic value of a flat accidental."
  -1)

(defmethod chromatic-offset ((accidental (eql '♮)))
  "Return the chromatic value of a natural accidental."
  0)

(defmethod chromatic-offset ((string string))
  "Return the chromatic offset disignated by a string."
  (loop
     :with offset = 0
     :for c :across string
     :do (incf offset (chromatic-offset (symbol-from-char c)))
     :finally (return offset)))

(defun print-chromatic-offset (offset stream)
  "Format a readable representation of a chromatic offset to a stream."
  (format stream "~v@{~A~:*~}" offset (if (plusp offset) #\♯ #\♭)))

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

(defmethod print-object ((pitch-class pitch-class) stream)
  "Print a readable representation of a pitch class to a stream."
  (princ (letter-name pitch-class) stream)
  (print-chromatic-offset (chromatic-offset pitch-class) stream))
  
(defmethod chromatic-value ((pitch-class pitch-class))
  "Return pitch the chromatic value of a pitch class."
  (+ (chromatic-value (letter-name pitch-class))
     (chromatic-offset pitch-class)))

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

(defmethod print-object ((note note) stream)
  "Print a readable representation of a note to a stream."
  (princ (pitch-class note) stream)
  (princ (octave note) stream))

(defun octave-midi-value (octave)
  "Return the midi offset of an octave."
  (* 12 (1+ octave)))

(defmethod midi-value ((note note))
  "Return the midi note value of a note."
  (+ (chromatic-value (pitch-class note))
     (octave-midi-value (octave note))))
