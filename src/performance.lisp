(in-package :music)

(defclass event ()
  ((note)
   (time)
   (duration))
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
