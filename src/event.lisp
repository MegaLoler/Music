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
    :accessor off-time)
   (channel
    :initarg :channel
    :initform 0
    :type (integer 0 15)
    :accessor channel))
  (:documentation "A concrete musical event, a note to play in real time."))

(defmethod print-object ((event event) stream)
  "Print an event object."
  (format stream "~A@~As-~As#~A"
	  (note event)
	  (on-time event)
	  (off-time event)
	  (channel event)))

(defmethod reference ((event event))
  "Return the note of an event as its own reference."
  (note event))

(defmethod event
    ((note note)
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (channel 0)
       (env (default-environment)))
  "Return an event from a note."
  (declare (ignore env))
  (make-instance
   'event
   :note note
   :on-time on-time
   :off-time (or off-time (+ on-time 1))
   :velocity velocity
   :channel channel))

(defmethod event
    (note
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (channel 0)
       (env (default-environment)))
  "Return an event from a realizable note."
  (unless (typep note 'musical-rest)
    (event (realize note env) on-time off-time velocity channel env)))

(defmethod event
    ((interval interval)
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (channel 0)
       (env (default-environment)))
  "Return two events from an interval."
  (event (list (reference env)
	       (above (reference env) interval))
	 on-time off-time velocity channel env))

(defmethod event
    ((key key)
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (channel 0)
       (env (default-environment)))
  "Return events for the notes of a scale."
  (event (seq (full-scale key)) on-time off-time velocity channel env))

(defmethod event
    ((notes list)
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (channel 0)
       (env (default-environment)))
  "Return a list of events."
  (loop
     :for note :in (realize notes env)
     :for on :in (if (consp on-time)
		     on-time
		     (make-list (length notes)
				:initial-element on-time))
     :for off :in (if (consp off-time)
		      off-time
		      (make-list (length notes)
				 :initial-element off-time))
     :collect (event note on off velocity channel env)))

(defmethod event
    ((chord chord)
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (channel 0)
       (env (default-environment)))
  "Return events for the notes of a chord."
  (loop
     :for note :in (objects (realize chord env))
     :for accent :in (accents chord)
     :collect (event note
		     on-time
		     off-time
		     (* accent velocity)
		     channel
		     env)))

(defmethod event
    ((voices voices)
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (channel 0)
       (env (default-environment)))
  "Return events for the notes of a group of voices."
  (loop
     :for note :in (objects (realize voices env))
     :for accent :in (accents voices)
     :for i :from 0
     :collect (event note
		     on-time
		     off-time
		     (* accent velocity)
		     (+ channel i)
		     env)))

(defmethod event
    ((seq seq)
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (channel 0)
       (env (default-environment)))
  "Return events for the notes of a sequence."
  (loop
     :with seq-beats = (if (listp (beats seq))
			(beats seq)
			(make-list (length (objects seq))
				   :initial-element (beats seq)))
     :for note :in (objects (realize seq env))
     :for beats :in seq-beats
     :for accent :in (accents seq)
     :for note-on-time :in (realize-on-time seq-beats on-time off-time env)
     :for note-off-time :in (realize-off-time seq-beats on-time off-time env)
     :for event = (event note
			 note-on-time
			 note-off-time
			 (* accent velocity)
			 channel
			 env)
     :when event :collect event))

(defmethod event
    ((song song)
     &optional
       (on-time 0)
       off-time
       (velocity 80)
       (channel 0)
       (env (default-environment)))
  "Return events for a song."
  (declare (ignore env)) ;; for now, later make it the parent env
  (event (body song) on-time off-time velocity channel
	 (make-instance
	  'environment
	  :meter (meter song)
	  :tempo (tempo song)
	  :key (key song))))
