(in-package :music)

;; TODO
;;   make environments have Parents, and able to shadow each others values
;;   possibly also give environments Content.. that or create a musical Packeg object or someting..

(defclass environment ()
  ((key
    :initarg :key
    :initform (key 'c-major)
    :type key
    :accessor key)
   (reference
    :initarg :reference
    :initform (note 'c4)
    :type note
    :accessor reference)
   (harmony
    :initarg :harmony
    :initform nil
    :type list
    :accessor harmony)
   (tempo
    :initarg :tempo
    :initform (make-tempo 120)
    :type tempo
    :accessor tempo)
   (meter
    :initarg :meter
    :initform (make-meter 4 4)
    :type meter
    :accessor meter))
  (:documentation "A musical environment in which musical objects are performed."))

(defclass event ()
  ((note
    :initarg :note
    :initform (note 'c4)
    :type note
    :accessor note)
   (on-time
    :initarg :on-time
    :initform 0
    :type real
    :accessor on-time)
   (off-time
    :initarg :off-time
    :initform 1
    :type real
    :accessor off-time))
  (:documentation "A concrete musical event."))

(defclass container ()
  ((objects
    :initarg :objects
    :initform nil
    :type list
    :accessor objects))
  (:documentation "An musical object containing other musical objects."))

(defclass chord (container) ()
  (:documentation "A vertical sequence of musical objects."))

(defclass seq (container) ()
  (:documentation "A horizontal sequence of musical objects."))

(defun make-chord (objects)
  "Make a chord from a list of musical objects."
  (make-instance 'chord :objects objects))

(defun make-seq (objects)
  "Make a sequence from a list of musical objects."
  (make-instance 'seq :objects objects))

(defmacro chord (&rest objects)
  "Make a chord."
  `(make-chord ',objects))

(defmacro seq (&rest objects)
  "Make a sequence."
  `(make-seq ',objects))

(defun default-environment ()
  "Return a default musical environment."
  (make-instance 'environment))

(defmethod clone ((env environment))
  "Clone a musical environment."
  (make-instance
   'environment
   :key       (key env)
   :reference (reference env)
   :harmony   (harmony env)
   :tempo     (tempo env)
   :meter     (meter env)))

(defmethod event (note value &optional (on-time 0) (env (default-environment)))
  "Realize an event from a note designator and a value designator in a musical environment."
  (make-instance
   'event
   :note (realize note env)
   :on-time (+ on-time (realize-on-time value env))
   :off-time (+ on-time (realize-off-time value env))))

(defmethod event ((notes list) (values list) &optional (on-time 0) (env (default-environment)))
  "Realize a list of events from a list of note designators and a list of value designators in a musical environment."
  (loop
     :for note :in (realize notes env)
     :for value :in values
     :for note-on-time :in (realize-on-time values env)
     :collect (event note value (+ on-time note-on-time))))

(defmethod print-object ((event event) stream)
  "Print an event object."
  (format stream "~A@~As-~As"
	  (note event)
	  (on-time event)
	  (off-time event)))
