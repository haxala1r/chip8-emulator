(defsystem :cl-chip8
  :name "cl-chip8"
  :version "0.1"
  :serial t
  :build-operation program-op
  :entry-point "cl-chip8::start"
  :depends-on ("sdl2" 
	       "mixalot"
	       "mixalot-mp3"
	       "uiop")
  :components ((:file "package")
	       (:file "program")
	       (:file "utils")
	       (:file "main")))
