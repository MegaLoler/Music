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
