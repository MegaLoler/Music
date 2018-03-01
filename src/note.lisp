(in-package :music)

(deftype letter-name ()
  "A letter name of a note."
  `(member a b c d e f g))

(defun letter-name-p (object)
  "Whether `object' is of type `letter-name'."
  (typep object 'letter-name))

;; is read-from-string a good idea here?
(defun letter-name (letter-name)
  "Return the letter name designated by `letter-name'."
  (let ((letter-name (read-from-string (string letter-name))))
    (if (letter-name-p letter-name)
	letter-name
	(error "Invalid letter name designator!"))))

(defun letter-name-chromatic-value (letter-name)
  "Return the integer representing the chromatic value of `letter-name'."
  (case (letter-name letter-name)
    (c 0)
    (d 2)
    (e 4)
    (f 5)
    (g 7)
    (a 9)
    (b 11)
    (otherwise (error "Invalid letter name!"))))

(defstruct (pitch-class (:print-object print-pitch-class))
  "Represents a pitch class."
  (letter-name 'c :type letter-name)
  (accidental 0 :type integer))

(defun accidental-to-string (accidental)
  "Format a readable representation of `accidental' to `stream'."
  (if (plusp accidental)
      (coerce (make-list accidental :initial-element #\♯) 'string)
      (coerce (make-list (- accidental) :initial-element #\♭) 'string)))

(defun print-pitch-class (pitch-class stream)
  "Print a readable representation of `pitch-class' to `stream'."
  (let ((pitch-class (pitch-class pitch-class)))
    (format stream "~S~A"
	    (pitch-class-letter-name pitch-class)
	    (accidental-to-string (pitch-class-accidental pitch-class)))))

(defun accidental-from-string (string)
  "Return the accidental value disignated by `string'."
  (reduce (lambda (acc character)
	    (+ acc (case character
		     (#\♯ 1)
		     (#\♭ -1)
		     (#\♮ 0)
		     (otherwise (error "Invalid accidental string!")))))
	  (coerce string 'list)
	  :initial-value 0))

;; should this be done in regex instead?
(defun pitch-class-from-string (string)
  "Return a pitch class designated by `string'."
  (let* ((characters (coerce (string string) 'list))
	 (letter-name (string (car characters)))
	 (accidental (coerce (cdr characters) 'string)))
    (make-pitch-class :letter-name (letter-name letter-name)
		      :accidental (accidental-from-string accidental))))

(defun pitch-class (pitch-class)
  "Return a pitch class designated by `pitch-class'."
  (if (pitch-class-p pitch-class)
      pitch-class
      (pitch-class-from-string pitch-class)))

(defun pitch-class-value (pitch-class)
  "Return pitch an integer representing the chromatic value of `pitch-class'."
  (+ (letter-name-chromatic-value (pitch-class-letter-name (pitch-class pitch-class)))
     (pitch-class-accidental (pitch-class pitch-class))))

(defstruct (note (:print-object print-note))
  "Represents an absolute musical note."
  (pitch-class (make-pitch-class) :type pitch-class)
  (octave 4 :type integer))

(defun print-note (note stream)
  "Print a readable representation of `note' to `stream'."
  (let ((note (note note)))
    (format stream "~S~S" (note-pitch-class note) (note-octave note))))

;; this seems messy, is it?
(defun note-from-string (string)
  "Return an absolute note designated by `string'."
  (let* ((characters (coerce (string string) 'list))
	 (tail-str (coerce (last characters) 'string))
	 (tail-val (read-from-string tail-str))
	 (octave-indicated (integerp tail-val))
	 (octave (if octave-indicated
		     tail-val
		     4))
	 (pitch-class-string (if octave-indicated
				 (coerce (subseq characters 0 (1- (length characters))) 'string)
				 (coerce characters 'string))))
    (make-note :pitch-class (pitch-class-from-string pitch-class-string)
	       :octave octave)))

(defun note (note)
  "Return a note designated by `note'."
  (if (note-p note)
      note
      (note-from-string note)))

(defun octave-midi-value (octave)
  "Return the midi offset of `octave'."
  (* 12 (1+ octave)))

(defun note-midi-value (note)
  "Return an integer representing the midi note value of `note'."
  (+ (pitch-class-value (note-pitch-class (note note)))
     (octave-midi-value (note-octave (note note)))))
