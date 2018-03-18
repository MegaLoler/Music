(in-package :music)

;; todo:
;;   able to read keys as major if their designators dont specify a mode

(deftype scale-degree ()
  `(member tonic supertonic mediant subdominant dominant submediant subtonic leading-tone))

(deftype solfège-syllable ()
  `(member do di ra re ri me mi fa fi se sol si le la li te ti))

(deftype mode ()
  `(member major minor harmonic-minor melodic-minor ionian dorian phrygian lydian mixolydian aeolian locrian))

(defmethod mode ((diatonic-value integer))
  "Return a mode of a diatonic value."
  (declare (type (integer 1 7) diatonic-value))
  (case diatonic-value
    (1 'ionian)
    (2 'dorian)
    (3 'phrygian)
    (4 'lydian)
    (5 'mixolydian)
    (6 'aeolian)
    (7 'locrian)))

(defmethod diatonic-value ((syllable (eql 'do))) 1)
(defmethod diatonic-value ((syllable (eql 'di))) 1)
(defmethod diatonic-value ((syllable (eql 'ra))) 2)
(defmethod diatonic-value ((syllable (eql 're))) 2)
(defmethod diatonic-value ((syllable (eql 'ri))) 2)
(defmethod diatonic-value ((syllable (eql 'me))) 3)
(defmethod diatonic-value ((syllable (eql 'mi))) 3)
(defmethod diatonic-value ((syllable (eql 'fa))) 4)
(defmethod diatonic-value ((syllable (eql 'fi))) 4)
(defmethod diatonic-value ((syllable (eql 'se))) 5)
(defmethod diatonic-value ((syllable (eql 'sol))) 5)
(defmethod diatonic-value ((syllable (eql 'si))) 5)
(defmethod diatonic-value ((syllable (eql 'le))) 6)
(defmethod diatonic-value ((syllable (eql 'la))) 6)
(defmethod diatonic-value ((syllable (eql 'li))) 6)
(defmethod diatonic-value ((syllable (eql 'te))) 7)
(defmethod diatonic-value ((syllable (eql 'ti))) 7)

(defmethod chromatic-value ((syllable (eql 'do))) 0)
(defmethod chromatic-value ((syllable (eql 'di))) 1)
(defmethod chromatic-value ((syllable (eql 'ra))) 1)
(defmethod chromatic-value ((syllable (eql 're))) 2)
(defmethod chromatic-value ((syllable (eql 'ri))) 3)
(defmethod chromatic-value ((syllable (eql 'me))) 3)
(defmethod chromatic-value ((syllable (eql 'mi))) 4)
(defmethod chromatic-value ((syllable (eql 'fa))) 5)
(defmethod chromatic-value ((syllable (eql 'fi))) 6)
(defmethod chromatic-value ((syllable (eql 'se))) 6)
(defmethod chromatic-value ((syllable (eql 'sol))) 7)
(defmethod chromatic-value ((syllable (eql 'si))) 8)
(defmethod chromatic-value ((syllable (eql 'le))) 8)
(defmethod chromatic-value ((syllable (eql 'la))) 9)
(defmethod chromatic-value ((syllable (eql 'li))) 10)
(defmethod chromatic-value ((syllable (eql 'te))) 10)
(defmethod chromatic-value ((syllable (eql 'ti))) 11)

(defmethod chromatic-offset ((syllable (eql 'do))) 0)
(defmethod chromatic-offset ((syllable (eql 'di))) 1)
(defmethod chromatic-offset ((syllable (eql 'ra))) -1)
(defmethod chromatic-offset ((syllable (eql 're))) 0)
(defmethod chromatic-offset ((syllable (eql 'ri))) 1)
(defmethod chromatic-offset ((syllable (eql 'me))) -1)
(defmethod chromatic-offset ((syllable (eql 'mi))) 0)
(defmethod chromatic-offset ((syllable (eql 'fa))) 0)
(defmethod chromatic-offset ((syllable (eql 'fi))) 1)
(defmethod chromatic-offset ((syllable (eql 'se))) -1)
(defmethod chromatic-offset ((syllable (eql 'sol))) 0)
(defmethod chromatic-offset ((syllable (eql 'si))) 1)
(defmethod chromatic-offset ((syllable (eql 'le))) -1)
(defmethod chromatic-offset ((syllable (eql 'la))) 0)
(defmethod chromatic-offset ((syllable (eql 'li))) 1)
(defmethod chromatic-offset ((syllable (eql 'te))) -1)
(defmethod chromatic-offset ((syllable (eql 'ti))) 0)

(defmethod diatonic-value ((mode (eql 'ionian))) 1)
(defmethod diatonic-value ((mode (eql 'dorian))) 2)
(defmethod diatonic-value ((mode (eql 'phrygian))) 3)
(defmethod diatonic-value ((mode (eql 'lydian))) 4)
(defmethod diatonic-value ((mode (eql 'mixolydian))) 5)
(defmethod diatonic-value ((mode (eql 'aeolian))) 6)
(defmethod diatonic-value ((mode (eql 'locrian))) 7)
(defmethod diatonic-value ((mode (eql 'major))) 1)
(defmethod diatonic-value ((mode (eql 'minor))) 6)
(defmethod diatonic-value ((mode (eql 'harmonic-minor))) 6)
(defmethod diatonic-value ((mode (eql 'melodic-minor))) 6)

(defmethod diatonic-value ((scale-degree (eql 'tonic))) 1)
(defmethod diatonic-value ((scale-degree (eql 'supertonic))) 2)
(defmethod diatonic-value ((scale-degree (eql 'mediant))) 3)
(defmethod diatonic-value ((scale-degree (eql 'subdominant))) 4)
(defmethod diatonic-value ((scale-degree (eql 'dominant))) 5)
(defmethod diatonic-value ((scale-degree (eql 'submediant))) 6)
(defmethod diatonic-value ((scale-degree (eql 'subtonic))) 7)
(defmethod diatonic-value ((scale-degree (eql 'leading-tone))) 7)

(defmethod tonic        ((mode (eql 'ionian))) (interval 'p1))
(defmethod supertonic   ((mode (eql 'ionian))) (interval 'ma2))
(defmethod mediant      ((mode (eql 'ionian))) (interval 'ma3))
(defmethod subdominant  ((mode (eql 'ionian))) (interval 'p4))
(defmethod dominant     ((mode (eql 'ionian))) (interval 'p5))
(defmethod submediant   ((mode (eql 'ionian))) (interval 'ma6))
(defmethod subtonic     ((mode (eql 'ionian))) (interval 'ma7))
(defmethod leading-tone ((mode (eql 'ionian))) (interval 'ma7))

(defmethod tonic        ((mode (eql 'dorian))) (interval 'p1))
(defmethod supertonic   ((mode (eql 'dorian))) (interval 'ma2))
(defmethod mediant      ((mode (eql 'dorian))) (interval 'mi3))
(defmethod subdominant  ((mode (eql 'dorian))) (interval 'p4))
(defmethod dominant     ((mode (eql 'dorian))) (interval 'p5))
(defmethod submediant   ((mode (eql 'dorian))) (interval 'ma6))
(defmethod subtonic     ((mode (eql 'dorian))) (interval 'mi7))
(defmethod leading-tone ((mode (eql 'dorian))) (interval 'ma7))

(defmethod tonic        ((mode (eql 'phrygian))) (interval 'p1))
(defmethod supertonic   ((mode (eql 'phrygian))) (interval 'mi2))
(defmethod mediant      ((mode (eql 'phrygian))) (interval 'ma3))
(defmethod subdominant  ((mode (eql 'phrygian))) (interval 'p4))
(defmethod dominant     ((mode (eql 'phrygian))) (interval 'p5))
(defmethod submediant   ((mode (eql 'phrygian))) (interval 'mi6))
(defmethod subtonic     ((mode (eql 'phrygian))) (interval 'mi7))
(defmethod leading-tone ((mode (eql 'phrygian))) (interval 'ma7))

(defmethod tonic        ((mode (eql 'lydian))) (interval 'p1))
(defmethod supertonic   ((mode (eql 'lydian))) (interval 'ma2))
(defmethod mediant      ((mode (eql 'lydian))) (interval 'ma3))
(defmethod subdominant  ((mode (eql 'lydian))) (interval 'a4))
(defmethod dominant     ((mode (eql 'lydian))) (interval 'p5))
(defmethod submediant   ((mode (eql 'lydian))) (interval 'ma6))
(defmethod subtonic     ((mode (eql 'lydian))) (interval 'ma7))
(defmethod leading-tone ((mode (eql 'lydian))) (interval 'ma7))

(defmethod tonic        ((mode (eql 'mixolydian))) (interval 'p1))
(defmethod supertonic   ((mode (eql 'mixolydian))) (interval 'ma2))
(defmethod mediant      ((mode (eql 'mixolydian))) (interval 'ma3))
(defmethod subdominant  ((mode (eql 'mixolydian))) (interval 'p4))
(defmethod dominant     ((mode (eql 'mixolydian))) (interval 'p5))
(defmethod submediant   ((mode (eql 'mixolydian))) (interval 'ma6))
(defmethod subtonic     ((mode (eql 'mixolydian))) (interval 'mi7))
(defmethod leading-tone ((mode (eql 'mixolydian))) (interval 'ma7))

(defmethod tonic        ((mode (eql 'aeolian))) (interval 'p1))
(defmethod supertonic   ((mode (eql 'aeolian))) (interval 'ma2))
(defmethod mediant      ((mode (eql 'aeolian))) (interval 'mi3))
(defmethod subdominant  ((mode (eql 'aeolian))) (interval 'p4))
(defmethod dominant     ((mode (eql 'aeolian))) (interval 'p5))
(defmethod submediant   ((mode (eql 'aeolian))) (interval 'mi6))
(defmethod subtonic     ((mode (eql 'aeolian))) (interval 'mi7))
(defmethod leading-tone ((mode (eql 'aeolian))) (interval 'ma7))

(defmethod tonic        ((mode (eql 'locrian))) (interval 'p1))
(defmethod supertonic   ((mode (eql 'locrian))) (interval 'mi2))
(defmethod mediant      ((mode (eql 'locrian))) (interval 'ma3))
(defmethod subdominant  ((mode (eql 'locrian))) (interval 'p4))
(defmethod dominant     ((mode (eql 'locrian))) (interval 'd5))
(defmethod submediant   ((mode (eql 'locrian))) (interval 'mi6))
(defmethod subtonic     ((mode (eql 'locrian))) (interval 'mi7))
(defmethod leading-tone ((mode (eql 'locrian))) (interval 'ma7))

(defmethod tonic        ((mode (eql 'major))) (interval 'p1))
(defmethod supertonic   ((mode (eql 'major))) (interval 'ma2))
(defmethod mediant      ((mode (eql 'major))) (interval 'ma3))
(defmethod subdominant  ((mode (eql 'major))) (interval 'p4))
(defmethod dominant     ((mode (eql 'major))) (interval 'p5))
(defmethod submediant   ((mode (eql 'major))) (interval 'ma6))
(defmethod subtonic     ((mode (eql 'major))) (interval 'ma7))
(defmethod leading-tone ((mode (eql 'major))) (interval 'ma7))

(defmethod tonic        ((mode (eql 'minor))) (interval 'p1))
(defmethod supertonic   ((mode (eql 'minor))) (interval 'ma2))
(defmethod mediant      ((mode (eql 'minor))) (interval 'mi3))
(defmethod subdominant  ((mode (eql 'minor))) (interval 'p4))
(defmethod dominant     ((mode (eql 'minor))) (interval 'p5))
(defmethod submediant   ((mode (eql 'minor))) (interval 'mi6))
(defmethod subtonic     ((mode (eql 'minor))) (interval 'mi7))
(defmethod leading-tone ((mode (eql 'minor))) (interval 'ma7))

(defmethod tonic        ((mode (eql 'harmonic-minor))) (interval 'p1))
(defmethod supertonic   ((mode (eql 'harmonic-minor))) (interval 'ma2))
(defmethod mediant      ((mode (eql 'harmonic-minor))) (interval 'mi3))
(defmethod subdominant  ((mode (eql 'harmonic-minor))) (interval 'p4))
(defmethod dominant     ((mode (eql 'harmonic-minor))) (interval 'p5))
(defmethod submediant   ((mode (eql 'harmonic-minor))) (interval 'mi6))
(defmethod subtonic     ((mode (eql 'harmonic-minor))) (interval 'mi7))
(defmethod leading-tone ((mode (eql 'harmonic-minor))) (interval 'ma7))

(defmethod tonic        ((mode (eql 'melodic-minor))) (interval 'p1))
(defmethod supertonic   ((mode (eql 'melodic-minor))) (interval 'ma2))
(defmethod mediant      ((mode (eql 'melodic-minor))) (interval 'mi3))
(defmethod subdominant  ((mode (eql 'melodic-minor))) (interval 'p4))
(defmethod dominant     ((mode (eql 'melodic-minor))) (interval 'p5))
(defmethod submediant   ((mode (eql 'melodic-minor))) (interval 'ma6))
(defmethod subtonic     ((mode (eql 'melodic-minor))) (interval 'ma7))
(defmethod leading-tone ((mode (eql 'melodic-minor))) (interval 'ma7))

(defclass key ()
  ((tonic
    :initarg :tonic
    :initform (pitch-class 'c)
    :type pitch-class
    :accessor tonic)
   (mode
    :initarg :mode
    :initform 'major
    :type mode
    :accessor mode)))

(defun make-key (tonic mode)
  "Construct a key with a tonal center and a mode."
  (make-instance 'key :tonic tonic :mode mode))

(defmethod key ((string string))
  "Return a key designated by a string."
  (with-input-from-string (stream string)
    (make-key (pitch-class (read-until "-" stream nil t))
	      (read stream nil))))

(defmethod key ((symbol symbol))
  "Return a key designated by a symbol."
  (key (string symbol)))

(defmethod key ((key key))
  "Return a key designated by itself."
  key)

(defmethod print-object ((key key) stream)
  "Print a key to a stream."
  (format stream "~A-~A" (tonic key) (mode key)))

(defmethod supertonic ((key key))
  "Get the supertonic of a key."
  (above (tonic key)
	 (supertonic (mode key))))

(defmethod mediant ((key key))
  "Get the mediant of a key."
  (above (tonic key)
	 (mediant (mode key))))

(defmethod subdominant ((key key))
  "Get the subdominant of a key."
  (above (tonic key)
	 (subdominant (mode key))))

(defmethod dominant ((key key))
  "Get the dominant of a key."
  (above (tonic key)
	 (dominant (mode key))))

(defmethod submediant ((key key))
  "Get the submediant of a key."
  (above (tonic key)
	 (submediant (mode key))))

(defmethod subtonic ((key key))
  "Get the subtonic of a key."
  (above (tonic key)
	 (subtonic (mode key))))

(defmethod leading-tone ((key key))
  "Get the leading-tone of a key."
  (above (tonic key)
	 (leading-tone (mode key))))

(defmethod scale-degree ((key key) (degree integer))
  "Get a scale degree of a key."
  (declare (type (integer 1 7) degree))
  (case degree
    (1 (tonic key))
    (2 (supertonic key))
    (3 (mediant key))
    (4 (subdominant key))
    (5 (dominant key))
    (6 (submediant key))
    (7 (subtonic key))))

(defmethod scale-degree ((key key) (syllable symbol))
  "Get a scale degree designated by a selfège syllable of a key."
  (declare (type solfège-syllable syllable))
  (above (tonic key)
	 (make-interval (diatonic-value syllable)
			(chromatic-value syllable))))

(defmethod scale ((key key))
  "Get the scale of a key."
  (loop
     :for degree :from 1 :to 7
     :collect (scale-degree key degree)))

(defmethod parallel ((key key) mode)
  "Get a parallel key of a key with a mode."
  (make-key (tonic key) mode))

(defmethod relative ((key key) mode)
  "Get a relative key of a key with a mode."
  (make-key
   (scale-degree
    key
    (subtract-diatonic-values
     (diatonic-value (mode key))
     (diatonic-value mode)))
   mode))

(defmethod resolve ((note note) (key key))
  "Return a pitch class in the context of a key."
  note)

(defmethod resolve ((pitch-class pitch-class) (key key))
  "Return a pitch class in the context of a key."
  pitch-class)

(defmethod resolve ((degree integer) (key key))
  "Return a pitch class in the context of a key."
  (scale-degree key degree))

(defmethod resolve ((degree symbol) (key key))
  "Return a pitch class in the context of a key."
  (if (typep degree '(or solfège-syllable scale-degree))
      (scale-degree key degree)
      (note-or-pitch-class degree)))
