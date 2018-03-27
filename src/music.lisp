(in-package :music)

;; TODO
;;   make environments have Parents, and able to shadow each others values
;;   possibly also give environments Content.. that or create a musical Closure object or someting..
;;   make a small accent dsl with X and * and > and < and buncha symbols for indicating accent levels
;;   with-meter, with-tempo, with-harmony

(defclass environment ()
  ((key
    :initarg  :key
    :initform (key 'c-major)
    :type     key
    :accessor key)
   (reference
    :initarg  :reference
    :initform (note 'c4)
    :type     note
    :accessor reference)
   (harmony
    :initarg  :harmony
    :initform nil
    :type     list
    :accessor env-harmony)
   (tempo
    :initarg  :tempo
    :initform (make-tempo 120)
    :type     tempo
    :accessor tempo)
   (meter
    :initarg  :meter
    :initform (make-meter 4 4)
    :type     meter
    :accessor meter))
  (:documentation "A musical environment in which musical objects are performed."))

(defclass container ()
  ((objects
    :initarg  :objects
    :initform nil
    :type     list
    :accessor objects)
   (accents
    :initarg  :accents
    :initform nil
    :type     list
    :accessor accents))
  (:documentation "An musical object containing other musical objects."))

(defclass chord (container) ()
  (:documentation "A vertical sequence of musical objects."))

(defclass voices (chord) ()
  (:documentation "A vertical sequence of musical objects representing individual voices."))

(defclass seq (container)
  ((beats
    :initarg  :beats
    :initform nil
    :type     list
    :accessor beats))
  (:documentation "A horizontal sequence of musical objects."))

(defmethod print-object ((chord chord) stream)
  "Print a chord."
  (prin1 `(chord (list ,@(objects chord))) stream))

(defmethod print-object ((voices voices) stream)
  "Print a group of voices."
  (prin1 `(voices (list ,@(objects voices))) stream))

(defmethod print-object ((seq seq) stream)
  "Print a sequence."
  (prin1 `(seq (list ,@(objects seq))) stream))

(defun chord (objects &optional accents)
  "Make a chord from a list of musical objects."
  (make-instance
   'chord
   :objects objects
   :accents (or accents (make-list (length objects) :initial-element 1))))

(defun voices (objects &optional accents)
  "Make a group of voices from a list of musical objects."
  (make-instance
   'voices
   :objects objects
   :accents (or accents (make-list (length objects) :initial-element 1))))

(defun seq (objects &optional beats accents)
  "Make a sequence from a list of musical objects."
  (make-instance
   'seq
   :objects objects
   :beats   (or beats (make-list (length objects) :initial-element 1))
   :accents (or accents (make-list (length objects) :initial-element 1))))

(defmethod note-count ((seq seq))
  "Get how many notes and rests there are in a sequence."
  (count-if (lambda (item)
	      (not (any-p item '(> <))))
	    (objects seq)))

(defmacro v (&rest objects)
  "Make a chord."
  `(chord ',objects))

(defmacro h (&rest objects)
  "Make a sequence."
  `(seq ',objects))

(defvar *global-environment* (make-instance 'environment))

(defun ref-env (note &optional (env (default-environment)))
  "Make a musical environment with a reference note."
  (let ((env (clone env)))
    (setf (reference env) (note note))
    env))

(defun default-environment ()
  "Return a default musical environment."
  *global-environment*)

(defmethod clone ((env environment))
  "Clone a musical environment."
  (make-instance
   'environment
   :key       (key env)
   :reference (reference env)
   :harmony   (env-harmony env)
   :tempo     (tempo env)
   :meter     (meter env)))

;; not sure if the reference in nested lists should be the first or last...
(defmethod reference ((list list))
  "Return the relevant reference note from a list of notes."
;  (reference (car (last (remove-if #'musical-rest-p list)))))
  (reference (car (remove-if #'musical-rest-p list))))

(defmethod reference ((chord chord))
  "Return the relevant reference note from a chord of notes."
  (reference (objects chord)))

(defmethod reference ((seq seq))
  "Return the relevant reference note from a sequence of notes."
  (reference (objects seq)))

(defmethod reference ((note note))
  "Return a note as its own reference."
  note)

(defmethod realize ((chord chord) &optional (env (default-environment)))
  "Realize a chord in a musical environment."
  (chord (realize (objects chord) env)
	 (accents chord)))

(defmethod realize ((voices voices) &optional (env (default-environment)))
  "Realize a group of voices in a musical environment."
  (voices (realize (objects voices) env)
	  (accents voices)))

(defmethod realize ((seq seq) &optional (env (default-environment)))
  "Realize a musical sequence in a musical environment."
  (seq (realize (objects seq) env)
       (beats seq)
       (accents seq)))

;; clean this up
(defmethod cat ((seq seq) &rest rest)
  "Concatenate musical sequences."
  (if rest
      (apply #'cat
	     (cons (seq (append (objects seq) (objects (car rest)))
			(if (and (numberp (beats seq))
				 (numberp (beats (car rest)))
				 (= (beats seq)
				    (beats (car rest))))
			    (beats seq)
			    (let ((a (if (listp (beats seq))
					 (beats seq)
					 (make-list (note-count seq)
						    :initial-element (beats seq))))
				  (b (if (listp (beats (car rest)))
					 (beats (car rest))
					 (make-list (note-count (car rest))
						    :initial-element (beats (car rest))))))
				  (append a b)))
			(append (accents seq) (accents (car rest))))
		   (cdr rest)))
      seq))

(defmethod cat ((chord chord) &rest rest)
  "Concatenate musical chords."
  (if rest
      (apply #'cat
	     (cat (chord (append (objects chord) (objects (car rest)))
			 (append (accents chord) (accents (car rest))))
		  (cdr rest)))
      chord))

;; this isn't quite right...
(defun join (&rest rest)
  "Join musical sequences in groupings."
  (concat (mapcar (lambda (seq)
		    (seq (list (objects seq))
			 (beats seq)
			 (accents seq)))
		 rest)))

(defun concat (items)
  "Apply `cat' to `items'."
  (apply #'cat items))

(defmethod repeat ((seq seq) &optional (n 2))
  "Repeat a musical sequence `n' times."
  (case n
    (0 (seq nil nil nil))
    (1 seq)
    (otherwise (cat seq (repeat seq (1- n))))))

(defmacro with-music-environment (env &body body)
  "Evaluate musical expressions in the context of a music environment."
  `(let ((*global-environment* ,env))
     ,@body))

(defmacro with-key (key &body body)
  "Evaluate musical expressions in the context of a key."
  `(with-music-environment (make-instance
			    `environment
			    :key (key ,key))
     ,@body))

(defmacro with-reference-note (note &body body)
  "Evaluate musical expressions in the context of a reference note."
  `(with-music-environment (make-instance
			    `environment
			    :reference (note ,note))
     ,@body))
