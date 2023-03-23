(in-package :cl-chip8)
; display etc. is handled in the main loop

(defparameter *screen-width* 640)
(defparameter *screen-height* 320)

(defparameter *main-delay* 1
  "The delay in main loop in number of milliseconds")

; This will be bound to a mixer when calling main-play
(defvar *main-mixer* nil)
(defparameter *beep-streamer* (mixalot-mp3:make-mp3-streamer "beep.mp3"))

; Key bindings. A single key is assigned to each virtual "key" 0-F
(defun intern-keyword (nm)
  "Interns keyword symbol from a given string name. Case Insensitive."
  (intern (string-upcase nm) :keyword))
(defmacro make-key-bindings (bindings)
  "Takes a list of keys and turns them to :scancode-<key> format."
  `(list
    ,@(loop for i in bindings collect `(intern-keyword (concatenate 'string "scancode-" (write-to-string ',i))))))
(defparameter *key-bindings*
  (make-key-bindings
   (x 1 2 3
      q w e a
      s d z c
      4 r f v))
  "A list of scancodes for each key. The key 'number' (hex 0-F) will be used as index to find its scancode. The weird layout is caused by chip-8 using a weird layout for its keypad. What you see here is it being recreated on a modern keyboard.")

(defparameter *key-states* (loop for i from 0 to 15 collect nil)
  "States of keys 0-F. This will be rebound using let for each program execution.")

(defmacro cond-all (&body body)
  "Like cond, but checks all conditions without exception."
  `(progn
     ,@(loop for i in body collect
             `(when ,(first i) ,@(rest i)))))

(defun display-program (rend screen)
  "Displays the given screen using the given sdl2 renderer.
   screen is expected to be a 32-by-64 array. 32 rows, 64 columns."
  (let ((pw (/ *screen-width* 64))
	(ph (/ *screen-height* 32)))
    (loop for r from 0 to 31 do
      (loop for c from 0 to 63 do
	(if (= (aref screen r c) 1)
	    (sdl2:set-render-draw-color rend 255 255 255 255)
	    (sdl2:set-render-draw-color rend 0 0 0 255))
	(sdl2:render-fill-rect rend
			       (sdl2:make-rect (* pw c) (* ph r) pw ph))))))

; you must first read-program and pass that to main-play.
(defun main-play (p mixer &optional (debug-mode nil))
  "Runs a given program with full emulated display and sound."
  (let ((total-time 0)
	(last-sound nil))
    (sdl2:with-init (:everything)
      (sdl2:with-window (win :title "CL-CHIP8" :flags '(:shown)
			     :w *screen-width* :h *screen-height*)
	(sdl2:with-renderer (rend win :flags '(:accelerated))
	  (sdl2:with-event-loop (:method :poll)
	    (:keyup
	     (:keysym keysym)
	     (if (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
		 (sdl2:push-event :quit))
	     (loop for i from 0 to 15 do
	       (when (sdl2:scancode= (sdl2:scancode-value keysym) (nth i *key-bindings*))
		 (setf (nth i *key-states*) nil)
		 (setf (program-last-pressed-key p) i))))
	    (:keydown
	     (:keysym keysym)
	     (loop for i from 0 to 15 do
	       (when (sdl2:scancode= (sdl2:scancode-value keysym) (nth i *key-bindings*))
		 (setf (nth i *key-states*) t))))
	    (:quit () t)
	    (:idle
	     ()
					; Clear screen.
	     (sdl2:set-render-draw-color rend 0 0 0 255)
	     (sdl2:render-clear rend)
					; Handle Delay and Sound timers.
	     (when (= (mod total-time 16) 0)
	       (cond-all
		 ((> (program-dt p) 0) (decf (program-dt p)))
		 ((> (program-st p) 0) (decf (program-st p))
		  (if (null last-sound)
		      (setf last-sound (mixalot:mixer-add-streamer mixer
								   (mixalot-mp3:make-mp3-streamer "beep.mp3")))
		      (mixalot:streamer-seek last-sound mixer 5000)))
		 ((= (program-st p) 0)
		  (unless (null last-sound)
		    (mixalot:mixer-remove-all-streamers mixer)
		    (setf last-sound nil)))))
					; Run instruction + handle errors
	     (handler-case
		 (run-instruction p debug-mode)
	       (invalid-instruction (err)
		 (print err)
		 (sdl2:push-event :quit))
	       (brk-cond (bp)
		 (print bp)
		 (sdl2:push-event :quit)))
	     (setf (program-last-pressed-key p) nil)
					; display screen. Maybe a good idea to not update the screen every instruction?
	     (display-program rend (program-screen p))
	     (sdl2:render-present rend)
	     (sdl2:delay *main-delay*)
	     (incf total-time *main-delay*))))))))


(defun main (filespec &optional (debug-mode nil))
  "Sets up a new disposable environment, and runs given program in main-play."
  (mixalot:main-thread-init)
  (let ((*main-mixer* (mixalot:create-mixer))
	(*key-states* *key-states*))
    (if (null *main-mixer*)
	(error "Cannot create mixer."))
    (handler-case 
	(main-play (read-program filespec) *main-mixer* debug-mode)
      (t (c)
	(format *standard-output* "An unexpected error occured.~%")
	(print c)))
    (mixalot:destroy-mixer *main-mixer*)))

(defun start ()
  "Parses command line arguments, sets quirks etc. and calls main."
  (main (first (uiop:command-line-arguments))))
