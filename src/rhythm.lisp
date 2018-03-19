(in-package :music)

;; TODO
;;   make note values their own class
;;   make tempo and meter designators!!!
;;   realize durations from Note Values (instead of explicit Beat values) and have keyword designators for them!! (whole, half, etc)

(defclass tempo ()
  ((bpm
   :initarg :bpm
   :initform 120
   :type real
   :accessor bpm)
  (beat-value
   :initarg :beat-value
   :initform 4
   :type integer
   :accessor beat-value)) ;; i think note value is gonna be its own class....
  (:documentation "A musical tempo."))

(defclass meter ()
  ((beats-per-measure
    :initarg :beats-per-measure
    :initform 4
    :type integer
    :accessor beats-per-measure)
   (beat-value
    :initarg :beat-value
    :initform 4
    :type integer
    :accessor beat-value))
  (:documentation "A musical meter."))

(defun make-tempo (bpm &optional (beat-value 4))
  "Construct a tempo object."
  (make-instance 'tempo :bpm bpm :beat-value beat-value))

(defun make-meter (beats-per-measure beat-value)
  "Construct a meter object."
  (make-instance
   'meter
   :beats-per-measure beats-per-measure
   :beat-value beat-value))

(defmethod realize-duration
    ((value real)
     &optional
       (on-time 0)
       off-time
       (env (default-environment)))
  "Realize a note duration from a beat value in a musical environment."
  (if off-time
      (- off-time on-time)
      (* 60 (/ (/ 1 value) (bpm (tempo env))))))

(defmethod realize-duration
    ((values list)
     &optional
       (on-time 0)
       off-time
       (env (default-environment)))
  "Realize multiple note durations in a musical environment."
  (if off-time
      (let* ((relative-lengths (mapcar #'reciprocal values))
	     (total (reduce #'+ relative-lengths))
	     (lengths (mapcar (lambda (length)
				(* (/ length total)
				   (- off-time on-time)))
			      relative-lengths)))
	lengths)
      (loop
	 :for value :in values
	 :collect (realize-duration value on-time off-time env))))

(defmethod duration
    ((value real)
     &optional
       (on-time 0)
       off-time
       (env (default-environment)))
  "Return the duration of a beat value in a musical environment."
  (realize-duration value on-time off-time env))

(defmethod duration
    ((values list)
     &optional
       (on-time 0)
       off-time
       (env (default-environment)))
  "Return the total duration of a list of beat values in a musical environment."
  (if off-time
      (- off-time on-time)
      (reduce #'+ (realize-duration values on-time off-time env))))

(defmethod realize-on-time
    ((value real)
     &optional
       (on-time 0)
       off-time
       (env (default-environment)))
  "Realize a relative on-time from a beat value in a musical environment."
  (declare (ignore off-time env))
  on-time)

(defmethod realize-off-time
    ((value real)
     &optional
       (on-time 0)
       off-time
       (env (default-environment)))
  "Realize a relative off-time from a beat value in a musical environment."
  (+ on-time (duration value on-time off-time env)))

(defmethod realize-on-time
    ((values list)
     &optional
       (on-time 0)
       off-time
       (env (default-environment)))
  "Return the relative on-times from a list of beat values in a musical environment."
  (loop
     :with time = on-time
     :with durations = (realize-duration values on-time off-time env)
     :for value :in values
     :for duration :in durations
     :collect (finc time duration)))

(defmethod realize-off-time
    ((values list)
     &optional
       (on-time 0)
       off-time
       (env (default-environment)))
  "Return the relative off-times from a list of beat values in a musical environment."
  (loop
     :with time = on-time
     :with durations = (realize-duration values on-time off-time env)
     :for value :in values
     :for duration :in durations
     :collect (incf time duration)))

(defun sum-beat-values (values)
  "Return the total beat value of a list of beat values."
  (/ 1 (apply #'+ (mapcar (lambda (value) (/ 1 value)) values))))
