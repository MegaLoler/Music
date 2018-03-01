(defsystem #:music
  :description "Playin with music stuff!"
  :author "MegaLoler"
  :serial t
  :components ((:module "src"
			:serial t
			:components
			((:file "package")
			 (:file "song")
			 (:file "note")))))
