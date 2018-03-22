(defpackage :nyan-cat
  (:use :cl :music)
  (:export nyan-cat))
(in-package :nyan-cat)

(defsong (nyan-cat
	  :title "Nyan Cat"
	  :description "It's Nyan Cat."
	  :key (key 'b-major)
	  :tempo (make-tempo 120)
	  :meter (make-meter 4 4))
  (flet ((intro () (seq '(> > > > mi fa sol r do sol mi fa sol do re mi re ti do r
			  sol r mi fa sol r do r re ti do re fa mi fa re < < <)
			4))
	 (m1-1 () (seq '(> sol r la r < ri mi re me re do r do r re r
			 me r me re do re mi sol la mi sol re mi do re do)
		       '(4 4 4 4 4 2 4 4 4 4 4 4 4 4 4
			 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4)))
	 (m1-2 (c) (seq `(mi r sol r la mi sol re mi do ri mi me re do re
			     me r do re mi sol re me re do re r do r ,@(if c '(do r) '(re r <)))
			4))
	 (m2-1 () (seq '(> do r sol la do r sol la do re mi do fa mi fa sol
			 < do r do r sol la do sol > fa mi re do < fa mi fa sol)
		       4))
	 (m2-2 (c) (seq `(do r sol la do r sol la do do re mi do sol la sol
			  do r do ti do sol la do fa mi fa sol < do r ,@(if c '(re r <) '(ti r <)))
			4))
	 (bass () (seq '(< < fa r > fa r < sol r > sol r < mi r > mi r < la r > la r
			 < < re r > re r < sol r > sol r < < do > do < do r re r mi r > >)
		       4)))
    (let ((melody (cat (intro)
		       (m1-1) (m1-2 nil)
		       (m1-1) (m1-2 t)
		       (m2-1) (m2-2 nil)
		       (m2-1) (m2-2 t)))
	  (bass (cat (seq '(r) 1/8) (repeat (bass) 8))))
      (list bass melody))))

;; (play nyan-cat)
