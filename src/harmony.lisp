(in-package :music)

;; TODO
;;   harmony constructor from roman numeral and chord notation
;;   give make-harmony ability to Alter members of the harmony!
;;   make build-harmony into a fancy macro that can take notes and/or intervals arbitrarily and stack them
;;   remake the harmony notation reader... use logic instead of hardcoding values!!
;;   make tertiary and beyond harmony notation work!!! those / / /  at the end
;;   fix reading letter names with accidentals in them...
;;   FIX SUSPENSIONS ><

(defmethod intervals ((q (eql 'major)) ext)                      '(ma3 p5))
(defmethod intervals ((q (eql 'minor)) ext)                      '(mi3 p5))
(defmethod intervals ((q (eql 'augmented)) ext)                  '(ma3 aug5))
(defmethod intervals ((q (eql 'diminished)) ext)                 '(mi3 dim5))

(defmethod intervals ((q (eql 'dominant)) ext)                   '(ma3 p5 mi7))
(defmethod intervals ((q (eql 'major)) (ext (eql 7)))            '(ma3 p5 ma7))
(defmethod intervals ((q (eql 'minor-major)) ext)                '(mi3 p5 ma7))
(defmethod intervals ((q (eql 'minor)) (ext (eql 7)))            '(mi3 p5 mi7))
(defmethod intervals ((q (eql 'augmented-major)) ext)            '(ma3 aug5 ma7))
(defmethod intervals ((q (eql 'augmented)) (ext (eql 7)))        '(ma3 aug5 mi7))
(defmethod intervals ((q (eql 'half-diminished)) ext)            '(mi3 dim5 mi7))
(defmethod intervals ((q (eql 'diminished)) (ext (eql 7)))       '(mi3 dim5 dim7))

(defmethod intervals ((q (eql 'major)) (ext (eql 9)))            '(ma3 p5 ma7 ma9))
(defmethod intervals ((q (eql 'dominant)) (ext (eql 9)))         '(ma3 p5 mi7 ma9))
(defmethod intervals ((q (eql 'minor-major)) (ext (eql 9)))      '(mi3 p5 ma7 ma9))
(defmethod intervals ((q (eql 'minor)) (ext (eql 9)))            '(mi3 p5 mi7 ma9))
(defmethod intervals ((q (eql 'augmented-major)) (ext (eql 9)))  '(ma3 aug5 ma7 ma9))
(defmethod intervals ((q (eql 'augmented)) (ext (eql 9)))        '(ma3 aug5 mi7 ma9))
(defmethod intervals ((q (eql 'half-diminished)) (ext (eql 9)))  '(mi3 dim5 mi7 ma9))
(defmethod intervals ((q (eql 'half-diminished-minor)) ext)      '(mi3 dim5 mi7 mi9))
(defmethod intervals ((q (eql 'diminished)) (ext (eql 9)))       '(mi3 dim5 dim7 ma9))
(defmethod intervals ((q (eql 'minor-diminished)) ext)           '(mi3 dim5 dim7 mi9))

(defmethod intervals ((q (eql 'dominant)) (ext (eql 11)))        '(ma3 p5 mi7 ma9 p11))
(defmethod intervals ((q (eql 'major)) (ext (eql 11)))           '(ma3 p5 ma7 ma9 p11))
(defmethod intervals ((q (eql 'minor-major)) (ext (eql 11)))     '(mi3 p5 ma7 ma9 p11))
(defmethod intervals ((q (eql 'minor)) (ext (eql 11)))           '(mi3 p5 mi7 ma9 p11))
(defmethod intervals ((q (eql 'augmented-major)) (ext (eql 11))) '(ma3 aug5 ma7 ma9 p11))
(defmethod intervals ((q (eql 'augmented)) (ext (eql 11)))       '(ma3 aug5 mi7 ma9 p11))
(defmethod intervals ((q (eql 'half-diminished)) (ext (eql 11))) '(mi3 dim5 mi7 mi9 p11))
(defmethod intervals ((q (eql 'diminished)) (ext (eql 11)))      '(mi3 dim5 dim7 mi9 dim11))

(defmethod intervals ((q (eql 'major)) (ext (eql 13)))           '(ma3 p5 ma7 ma9 p11 ma13))
(defmethod intervals ((q (eql 'dominant)) (ext (eql 13)))        '(ma3 p5 mi7 ma9 p11 ma13))
(defmethod intervals ((q (eql 'minor-major)) (ext (eql 13)))     '(mi3 p5 ma7 ma9 p11 ma13))
(defmethod intervals ((q (eql 'minor)) (ext (eql 13)))           '(mi3 p5 mi7 ma9 p11 ma13))
(defmethod intervals ((q (eql 'augmented-major)) (ext (eql 13))) '(ma3 aug5 ma7 ma9 p11 ma13))
(defmethod intervals ((q (eql 'augmented)) (ext (eql 13)))       '(ma3 aug5 mi7 ma9 p11 ma13))
(defmethod intervals ((q (eql 'half-diminished)) (ext (eql 13))) '(mi3 dim5 mi7 ma9 p11 ma13))

(defmethod intervals ((q (eql 'maj)) ext) (intervals 'major ext))
(defmethod intervals ((q (eql 'ma)) ext) (intervals 'major ext))
(defmethod intervals ((q (eql 'mj)) ext) (intervals 'major ext))
(defmethod intervals ((q (eql 'j)) ext) (intervals 'major ext))
(defmethod intervals ((q (eql 'Δ)) ext) (intervals 'major ext))
(defmethod intervals ((q (eql 'min)) ext) (intervals 'minor ext))
(defmethod intervals ((q (eql 'mi)) ext) (intervals 'minor ext))
(defmethod intervals ((q (eql 'mn)) ext) (intervals 'minor ext))
(defmethod intervals ((q (eql '−)) ext) (intervals 'minor ext))
(defmethod intervals ((q (eql '-)) ext) (intervals 'minor ext))
(defmethod intervals ((q (eql 'aug)) ext) (intervals 'augmented ext))
(defmethod intervals ((q (eql 'au)) ext) (intervals 'augmented ext))
(defmethod intervals ((q (eql 'ag)) ext) (intervals 'augmented ext))
(defmethod intervals ((q (eql 'a)) ext) (intervals 'augmented ext))
(defmethod intervals ((q (eql '+)) ext) (intervals 'augmented ext))
(defmethod intervals ((q (eql 'dim)) ext) (intervals 'diminished ext))
(defmethod intervals ((q (eql 'di)) ext) (intervals 'diminished ext))
(defmethod intervals ((q (eql 'dm)) ext) (intervals 'diminished ext))
(defmethod intervals ((q (eql 'd)) ext) (intervals 'diminished ext))
(defmethod intervals ((q (eql 'o)) ext) (intervals 'diminished ext))
(defmethod intervals ((q (eql '°)) ext) (intervals 'diminished ext))
(defmethod intervals ((q (eql 'dom)) ext) (intervals 'dominant ext))
(defmethod intervals ((q (eql 'half-dim)) ext) (intervals 'half-diminished ext))
(defmethod intervals ((q (eql 'half-di)) ext) (intervals 'half-diminished ext))
(defmethod intervals ((q (eql 'half-dm)) ext) (intervals 'half-diminished ext))
(defmethod intervals ((q (eql 'half-d)) ext) (intervals 'half-diminished ext))
(defmethod intervals ((q (eql 'h-dim)) ext) (intervals 'half-diminished ext))
(defmethod intervals ((q (eql 'h-di)) ext) (intervals 'half-diminished ext))
(defmethod intervals ((q (eql 'h-dm)) ext) (intervals 'half-diminished ext))
(defmethod intervals ((q (eql 'h-d)) ext) (intervals 'half-diminished ext))
(defmethod intervals ((q (eql 'hdim)) ext) (intervals 'half-diminished ext))
(defmethod intervals ((q (eql 'hdi)) ext) (intervals 'half-diminished ext))
(defmethod intervals ((q (eql 'hdm)) ext) (intervals 'half-diminished ext))
(defmethod intervals ((q (eql 'hd)) ext) (intervals 'half-diminished ext))
(defmethod intervals ((q (eql 'min-maj)) ext) (intervals 'minor-major ext))
(defmethod intervals ((q (eql 'mi-ma)) ext) (intervals 'minor-major ext))
(defmethod intervals ((q (eql 'mn-mj)) ext) (intervals 'minor-major ext))
(defmethod intervals ((q (eql 'mn-j)) ext) (intervals 'minor-major ext))
(defmethod intervals ((q (eql 'm-m)) ext) (intervals 'minor-major ext))
(defmethod intervals ((q (eql 'm-j)) ext) (intervals 'minor-major ext))
(defmethod intervals ((q (eql 'minmaj)) ext) (intervals 'minor-major ext))
(defmethod intervals ((q (eql 'mima)) ext) (intervals 'minor-major ext))
(defmethod intervals ((q (eql 'mnmj)) ext) (intervals 'minor-major ext))
(defmethod intervals ((q (eql 'mnj)) ext) (intervals 'minor-major ext))
(defmethod intervals ((q (eql 'mj)) ext) (intervals 'minor-major ext))
(defmethod intervals ((q (eql 'mm)) ext) (intervals 'minor-major ext))
(defmethod intervals ((q (eql 'aug-maj)) ext) (intervals 'augmented-major ext))
(defmethod intervals ((q (eql 'au-ma)) ext) (intervals 'augmented-major ext))
(defmethod intervals ((q (eql 'ag-mj)) ext) (intervals 'augmented-major ext))
(defmethod intervals ((q (eql 'ag-j)) ext) (intervals 'augmented-major ext))
(defmethod intervals ((q (eql 'a-m)) ext) (intervals 'augmented-major ext))
(defmethod intervals ((q (eql 'a-j)) ext) (intervals 'augmented-major ext))
(defmethod intervals ((q (eql 'augmaj)) ext) (intervals 'augmented-major ext))
(defmethod intervals ((q (eql 'auma)) ext) (intervals 'augmented-major ext))
(defmethod intervals ((q (eql 'agmj)) ext) (intervals 'augmented-major ext))
(defmethod intervals ((q (eql 'agj)) ext) (intervals 'augmented-major ext))
(defmethod intervals ((q (eql 'am)) ext) (intervals 'augmented-major ext))
(defmethod intervals ((q (eql 'aj)) ext) (intervals 'augmented-major ext))

(defmethod intervals (q ext)            (intervals 'major ext))
(defmethod intervals (q (ext (eql 7)))  (intervals 'dominant ext))
(defmethod intervals (q (ext (eql 9)))  (intervals 'major ext))
(defmethod intervals (q (ext (eql 11))) (intervals 'dominant ext))
(defmethod intervals (q (ext (eql 11))) (intervals 'dominant ext))

(defmethod extension ((inversion (eql 53)))  5)
(defmethod extension ((inversion (eql 63)))  5)
(defmethod extension ((inversion (eql 64)))  5)
(defmethod extension (inversion)             5)
(defmethod extension ((inversion (eql 6)))   5)

(defmethod extension ((inversion (eql 753))) 7)
(defmethod extension ((inversion (eql 653))) 7)
(defmethod extension ((inversion (eql 643))) 7)
(defmethod extension ((inversion (eql 642))) 7)
(defmethod extension ((inversion (eql 7)))   7)
(defmethod extension ((inversion (eql 75)))  7)
(defmethod extension ((inversion (eql 65)))  7)
(defmethod extension ((inversion (eql 43)))  7)
(defmethod extension ((inversion (eql 42)))  7)
(defmethod extension ((inversion (eql 2)))   7)

(defmethod extension ((inversion (eql 9)))   9)
(defmethod extension ((inversion (eql 11)))  11)
(defmethod extension ((inversion (eql 13)))  13)

(defmethod inversion ((inversion (eql 53)))  0)
(defmethod inversion ((inversion (eql 63)))  1)
(defmethod inversion ((inversion (eql 64)))  2)
(defmethod inversion (inversion)             0)
(defmethod inversion ((inversion (eql 6)))   1)

(defmethod inversion ((inversion (eql 753))) 0)
(defmethod inversion ((inversion (eql 653))) 1)
(defmethod inversion ((inversion (eql 643))) 2)
(defmethod inversion ((inversion (eql 642))) 3)
(defmethod inversion ((inversion (eql 7)))   0)
(defmethod inversion ((inversion (eql 75)))  0)
(defmethod inversion ((inversion (eql 65)))  1)
(defmethod inversion ((inversion (eql 43)))  2)
(defmethod inversion ((inversion (eql 42)))  3)
(defmethod inversion ((inversion (eql 2)))   3)

(defmethod inversion ((inversion (eql 9)))   0)
(defmethod inversion ((inversion (eql 11)))  0)
(defmethod inversion ((inversion (eql 13)))  0)

(defmethod suspension (sus)               nil)
(defmethod suspension ((sus (eql 'sus2))) 2)
(defmethod suspension ((sus (eql 'sus4))) 4)

(defmethod additions (add)                 nil)
(defmethod additions ((add (eql 'add2)))  '(ma2))
(defmethod additions ((add (eql 'add4)))  '(p4))
(defmethod additions ((add (eql 'add9)))  '(ma9))
(defmethod additions ((add (eql 'add11))) '(p11))
(defmethod additions ((add (eql 'add13))) '(ma13))

(defun ext-to-members (ext)
  "Return how many chord members in a chord of some extension."
  (/ (1+ ext) 2))

(defun make-harmony
    (&key
       (key (key 'c-major))
       quality
       (root 1)
       (inversion 0)
       (members 3)
       (interval 3)
       (offset 0)
       alterations
       additions
       suspension)
  "Return a set of pitch classes of a harmony built from the scale of a key."
  (declare (type (member nil 2 4) suspension))
  (invert (loop
	     :with key = (key key)
	     :with root-degree = (degree key root)
	     :for degree :from root-degree :by (1- interval)
	     :for alteration :in (or alterations
				     (make-list members :initial-element 0))
	     :for n :from 1
	     :for mod = (diatonic-class degree)
	     :for interval :in (if quality
				   (append (list 'p1)
					   (intervals quality (1- (* 2 members))))
				   (make-list members :initial-element nil))
	     :collect (above (if interval
				 (above (scale-degree key (if (and suspension (= n 2))
							      (+ (add-diatonic-values
								  root-degree
								  (diatonic-class suspension)))
							      root-degree))
					(interval interval))
				 (scale-degree key (if (and suspension (= n 2))
						       (+ (add-diatonic-values
							   root-degree
							   (diatonic-class suspension)))
						       mod)))
			     (make-interval 1 (+ offset alteration)))
	     :into result
	     :finally (return
			(append
			 result
			 (mapcar
			  (lambda (degree)
			    (scale-degree
			     key
			     (+ (add-diatonic-values
				 root-degree
				 (diatonic-class degree)))))
			  additions))))
	     inversion))

(defun build-harmony (root &rest intervals)
  "Return a set of pitch classes or notes of a harmony given a root and a set of intervals."
  (let ((root (note-or-pitch-class root)))
    (mapcar (lambda (interval)
	      (above root (interval interval)))
	    (cons (interval 'p1) intervals))))

(defun invert (harmony &optional (count 1))
  "Invert a harmony."
  (cond ((plusp count)
	 (invert (rotate-left harmony) (1- count)))
	((minusp count)
	 (invert (rotate-right harmony) (1+ count)))
	(t harmony)))

(defmethod read-chord-root ((symbol symbol) (key key))
  "Read a chord root designated by a symbol."
  (if (typep symbol '(or solfège-syllable scale-degree roman-numeral number))
      (scale-degree key symbol)
      (pitch-class symbol)))

(defmethod read-chord-root ((string string) (key key))
  "Read a chord root designated by a string."
  (read-chord-root (symbol-from-char string) key))

(defmethod read-chord-root ((number number) (key key))
  "Read a chord root designated by a number."
  (scale-degree key number))

(defun read-secondary (string key)
  "Read a secondary chord root designated by a string."
  (read-chord-root (if (> (length string) 0)
		       (read-from-string (subseq string 1))
		       'do)
		   key))

(defmethod read-altered ((string string))
  "Read chord alterations designated by a string."
  (with-input-from-string (stream string)
    (loop
       :with alterations = (make-list 7 :initial-element 0)
       :for c = (read-char stream nil nil)
       :while c
       :do (cond ((any-p c "♯#+")
		  (incf (nth (/ (1- (parse-integer (string (read-char stream)))) 2)
			     alterations)))
		 ((any-p c "♭b°oOdD")
		  (decf (nth (/ (1- (parse-integer (string (read-char stream)))) 2)
			     alterations))))
       :finally (return alterations))))

(defmethod read-altered ((symbol symbol))
  "Read chord alterations designated by a symbol."
  (read-altered (string symbol)))

(defmethod harmony ((string string) &optional (env (default-environment)))
  "Make a harmony designated by a string."
  (with-input-from-string (stream string)
    (let* ((accidental (chromatic-offset (read-until-not "♯#♭b♮" stream)))
	   (root-designator (concatenate 'string
	   				 (string (read-char stream))
	   				 (read-until "/0123456789♯#♭b♮mMdDaAsS+-−°oOøØΔhH"
	   					     stream)))
	   (quality (symbol-from-char (read-until "/123456789♯#♭bsS" stream) nil))
	   (inversion-extension (read-until "/♯#♭baAsS°+oO" stream))
	   (added (read-until "/♯#♭b°+oO" stream))
	   (altered (read-altered (read-until "/" stream)))
	   (secondary (read-secondary (read-line stream nil "") (key env)))
	   (secondary-key (secondary (key env) secondary))
	   (root (read-chord-root root-designator secondary-key))
      	   (ie (if (> (length inversion-extension) 0)
      		   (parse-integer inversion-extension)
      		   nil))
      	   (inversion (inversion ie))
      	   (extension (extension ie))
      	   (members (max (ext-to-members extension)
      			 (length (intervals quality extension))))
      	   (suspension (suspension (symbol-from-char added nil)))
      	   (additions (additions (symbol-from-char added nil))))
      (make-harmony :key (key env)
      		    :root root
      		    :quality (or quality (> (length inversion-extension) 0))
      		    :inversion inversion
      		    :members members
      		    :offset accidental
      		    :alterations altered
      		    :additions additions
      		    :suspension suspension))))

(defmethod harmony ((symbol symbol) &optional (env (default-environment)))
  "Make a harmony designated by a symbol."
  (harmony (string symbol) env))

(defmethod harmony ((harmonies list) &optional (env (default-environment)))
  "Make a list of harmonies by a list of designators."
  (mapcar (lambda (harmony) (harmony harmony env)) harmonies))
