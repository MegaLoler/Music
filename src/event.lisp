(in-package :music)

(defclass event ()
  ((note
    :initarg :note
    :initform (note 'c4)
    :type note
    :accessor note)
   (velocity
    :initarg :velocity
    :initform 80
    :type velocity
    :accessor velocity)
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
  (:documentation "A concrete musical event, a note to play in real time."))

(defmethod print-object ((event event) stream)
  "Print an event object."
  (format stream "~A@~As-~As"
	  (note event)
	  (on-time event)
	  (off-time event)))

(defmethod reference ((event event))
  "Return the note of an event as its own reference."
  (note event))

(defmethod event
    ((note note)
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (env (default-environment)))
  "Return an event from a note."
  (declare (ignore env))
  (make-instance
   'event
   :note note
   :on-time on-time
   :off-time (or off-time (+ on-time 1))
   :velocity velocity))

(defmethod event
    (note
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (env (default-environment)))
  "Return an event from a realizable note."
  (event (realize note env) on-time off-time velocity env))

(defmethod event
    ((interval interval)
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (env (default-environment)))
  "Return two events from an interval."
  (event (list (reference env)
	       (above (reference env) interval))
	 on-time off-time velocity env))

(defmethod event
    ((key key)
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (env (default-environment)))
  "Return events for the notes of a scale."
  (event (seq (full-scale key)) on-time off-time velocity env))

(defmethod event
    ((notes list)
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (env (default-environment)))
  "Return a list of events."
  (loop
     :for note :in (realize notes env)
     :collect (event note on-time off-time velocity env)))

(defmethod event
    ((chord chord)
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (env (default-environment)))
  "Return events for the notes of a chord."
  (loop
     :for note :in (objects (realize chord env))
     :for accent :in (accents chord)
     :collect (event note
		     on-time
		     off-time
		     (* accent velocity)
		     env)))

(defmethod event
    ((seq seq)
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (env (default-environment)))
  "Return events for the notes of a sequence."
  (loop
     :for note :in (objects (realize seq env))
     :for beats :in (beats seq)
     :for accent :in (accents seq)
     :for note-on-time :in (realize-on-time (beats seq) on-time off-time env)
     :for note-off-time :in (realize-off-time (beats seq) on-time off-time env)
     :for event = (event note
			 note-on-time
			 note-off-time
			 (* accent velocity)
			 env)
     :collect event))
