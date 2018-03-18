(in-package :music)

;; todo:
;;   optimize the play methods for Lists so they use play-noteS instead of lots of play-note
;;     you know, since the scheduling is relative and running fewer timers would be a lot better for synchronization -- mainly just check and see if its a list of Events, and in that case, put them in parallel, and else just do Play Notes

(defvar *midi-out* nil)

(defun init-midi (&optional (out-id (pm:get-default-output-device-id)))
  "Initialize midi output with portmidi."
  (if *midi-out*
      (error "A midi stream is already open!")
      (setf *midi-out* (pm:open-output out-id 1024 0))))

(defun close-midi ()
  "Close the open midi connection with portmidi."
  (if *midi-out*
      (setf *midi-out* (pm:close-midi *midi-out*))
      (error "A midi stream is not open yet!")))

(defun ensure-midi ()
  "Make sure a midi stream is available."
  (unless *midi-out* (init-midi)))

(defun note-on (value &optional (velocity 80) (channel 0) (stream *midi-out*))
  "Play a midi note."
  (pm:write-short-midi stream 0 (pm:note-on channel value velocity)))

(defun note-off (value &optional (channel 0) (stream *midi-out*))
  "Stop a midi note."
  (pm:write-short-midi stream 0 (pm:note-off channel value 0)))

(defun notes-on (values &optional (velocity 80) (channel 0) (stream *midi-out*))
  "Play multiple midi notes."
  (loop :for value :in values :do (note-on value velocity channel stream)))

(defun notes-off (values &optional (channel 0) (stream *midi-out*))
  "Stop multiple midi notes."
  (loop :for value :in values :do (note-off value channel stream)))

(defun panic (&optional (channel 0) (stream *midi-out*))
  "Stop all notes on a channel of a midi stream."
  (loop :for n :from 0 :to 127 :do (note-off n channel stream)))

(defun schedule (time fn &rest args)
  "Schedule a function to be called after some amount of seconds."
  (schedule-timer
   (make-timer
    (lambda ()
      (apply fn args))
    :thread t)
   time))

(defun play-note (note &optional (on-time 0) off-time (velocity 80))
  "Play a note."
  (ensure-midi)
  (let ((value (chromatic-value (note note)))
	(off-time (or off-time (+ on-time 1))))
    (schedule on-time #'note-on value velocity)
    (schedule off-time #'note-off value)))

(defun play-notes (notes &optional (on-time 0) off-time (velocity 80))
  "Play multiple notes."
  (ensure-midi)
  (let ((values (mapcar (lambda (note) (chromatic-value (note note))) notes))
	(off-time (or off-time (+ on-time 1))))
    (schedule on-time #'notes-on values velocity)
    (schedule off-time #'notes-off values)))

(defmethod play ((event event))
  "Play an event."
  (play-note (note event)
	     (on-time event)
	     (off-time event)
	     (velocity event)))

(defmethod play ((note note))
  "Play a note."
  (play-note note))

(defmethod play ((interval interval))
  "Play an interval."
  (let ((reference (reference (default-environment))))
    (play-notes (list reference
		      (above reference interval)))))

(defmethod play (object)
  "Play a realizable object."
  (play (realize object)))

(defmethod play ((chord chord))
  "Play a chord."
  (play (event chord 1)))

(defmethod play ((seq seq))
  "Play a sequence."
  (play (event seq 1)))

(defmethod play ((list list))
  "Play a list of objects."
  (mapc #'play list))
