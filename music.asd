(defsystem #:music
  :description "Playin with music stuff!"
  :author "MegaLoler"
  :serial t
  :components ((:module "src"
			:serial t
			:components
			((:file "package")
			 (:file "util")
			 (:file "song")
			 (:file "interval")
			 (:file "note")
			 (:file "scale")
			 (:file "harmony")
			 (:file "play")))))
