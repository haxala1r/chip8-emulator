(in-package :cl-chip8)

; This data structure holds the memory, registers and
; other state of a given program.
(defstruct program
  (pc #x200)
  (stack nil)
  (dt 0) ; delay timer
  (st 0) ; sound timer
  i ; the i register.
  regs ; general purpose registers
  mem
  ; the screen is a 64-by-32 array of pixels (either 0 or 1) 
  screen
  ; Breakpoints
  (brks nil)
  ; This is kind of special - This is updated every time a key is pressed, but
  ; set back to nil after every instruction. Only used when FX0A is executed, 
  ; since that instruction has to wait for a key press.
  last-pressed-key)

(defun v (x program)
  "Get the VX register."
  (if (or (< x 0)
	  (> x 15))
      (error "Get VX error: out of bounds."))
  (aref (program-regs program) x))

(defun set-register (p x n)
  (setf (aref (program-regs p) x) n))

(defun inc-pc (p)
  (incf (program-pc p) 2))

(setf *read-base* 16)
(defvar *digit-sprites*
  #(F0 90 90 90 F0 ;0
    20 60 20 20 70 ;1
    F0 10 F0 80 F0 ;2
    F0 10 F0 10 F0 ;3
    90 90 F0 10 10 ;4
    F0 80 F0 10 F0 ;5
    F0 80 F0 90 F0 ;6
    F0 10 20 40 40 ;7
    F0 90 F0 90 F0 ;8
    F0 90 F0 10 F0 ;9
    F0 90 F0 90 90 ;A
    E0 90 E0 90 E0 ;B
    F0 80 80 80 F0 ;C
    E0 90 90 90 E0 ;D
    F0 80 F0 80 F0 ;E
    F0 80 F0 80 80)) ;F
(setf *read-base* #xa)

(defun setup-memory ()
  (let ((mem (make-array #x1000
			 :element-type 'unsigned-byte
			 :adjustable nil
			 :initial-element 0)))
    (loop for i from 0 to (- (length *digit-sprites*) 1)
	  do (setf (aref mem i) (aref *digit-sprites* i)))
    mem))

(defun read-program (filespec)
  "Reads a given file as a chip-8 program.
   First creates a fresh array of 0x1000 bytes (4 KiB) as memory for our program, then
   reads the file writing to address 0x200 and above."
  (let ((mem (setup-memory))
	(screen (make-array '(32 64)
			    :initial-element 0
			    :element-type 'bit
			    :adjustable nil)))
    ; read from file
    (with-open-file (st filespec :element-type 'unsigned-byte)
      (read-sequence mem st :start #x200 :end #x1000))
    ; return the program object
    (make-program :regs (make-array 16 :element-type 'unsigned-byte :initial-element 0)
		  :mem mem
		  :screen screen)))

(define-condition invalid-instruction (error)
  ((instruction
    :initarg :ins
    :reader ins)
   (text
    :initarg :text
    :reader text
    :initform "Invalid instruction.")
   (program
    :initarg :program
    :reader program)))

(defmethod print-object ((object invalid-instruction) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "Error: ~a~%Adress: 0x~3,'0X~%Full instruction: ~4,'0X"
	    (text object) (program-pc (program object)) (ins object))))

(define-condition brk-cond ()
  ((program
    :initarg :program
    :reader brk-program)
   (address
    :initarg :addr
    :reader addr)))

(defmethod print-object ((object brk-cond) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "Breakpoint at ~3,'0X was hit." (addr object))))

(defparameter *enabled-quirks* nil)
(defparameter *implemented-quirks*
  (list :shift   ; Makes 8XY6 and 8XYE shift VX instead and completely ignore VY.
	:clearvf ; Makes 8XY1 8XY2 and 8XY3 clear VF.
	))

(defun enable-quirk (q)
  (unless (member q *enabled-quirks*)
    (push q *enabled-quirks*)))

(defun disable-quirk (q)
  (if (member q *enabled-quirks*)
      (delete q *enabled-quirks*)))

(defun enabled-p (quirk)
  "Checks if given quirk is currently enabled."
  (member quirk *enabled-quirks*))

; Here are the full instruction handlers. It isn't the most elegant way, but
; I decided to organise instructions into functions determined by their most
; significant nibble, since that seems to be a good seperator for chip-8.

(defmacro defins (num &body body)
  "A macro that defines an instruction handler function.
   This is only here because all instruction handlers require the same parameters and
   follow a common naming scheme.
   Also, MSB (most significant byte) and LSB (least significant byte) are defined
   by the macro instead of being passed via arguments."
  `(defun ,(intern (concatenate 'string "INS-" (write-to-string num)))
       (p full-ins)
     (let ((msb (ash full-ins -8))
	   (lsb (logand full-ins #xFF)))
       ; these are here to prevent unused argument warnings
       p msb lsb
       ,@body)))

; 0 - Return and CLS are here.
(defins 0
  (case full-ins
    (#x00E0 ; 00E0 : Clear display
     (loop for r from 0 to 31 do
       (loop for c from 0 to 63 do
	 (setf (aref (program-screen p) r c) 0)))
     (inc-pc p))
    (#x00EE ; 00EE : Return
     (if (null (program-stack p))
	 (error 'invalid-instruction :text "Cannot execute return (0x00EE), stack empty"
				     :ins full-ins :program p))
     (setf (program-pc p) (pop (program-stack p))))
    (t (error 'invalid-instruction :text "The instruction 0nnn (SYS) is not supported"
				   :ins full-ins :program p))))

; 1NNN : Jump to address NNN
(defins 1
  (setf (program-pc p)
	(logand full-ins #xFFF)))

; 2NNN : Call subroutine at NNN
(defins 2
  (inc-pc p)
  (push (program-pc p) (program-stack p))
  (setf (program-pc p)
	(logand full-ins #xFFF)))

; 3XNN : compare VX to NN, skip next instruction if equal.
(defins 3
  (if (= (v (logand msb #xF) p) lsb)
      (inc-pc p))
  (inc-pc p))

; 4XNN : compare VX to NN, skip next instruction if not equal.
(defins 4
  (if (/= (v (logand msb #xF) p) lsb)
      (inc-pc p))
  (inc-pc p))

; 5XY0 : compare VX to VY, skip next instruction if equal.
(defins 5
  (if (= (v (logand msb #xF) p)
	 (v (ash lsb -4) p))
      (inc-pc p))
  (inc-pc p))

; 6XNN : Set VX to NN
(defins 6
  (set-register p (logand msb #xF) lsb)
  (inc-pc p))

; 7XNN : Add NN to VX and store it in VX
(defins 7
  (set-register p (logand msb #xF)
		     (mod
		      (+ (v (logand msb #xF) p) lsb)
		      #x100))
  (inc-pc p))

; The 8--- set of instructions, determined by least-significant nibble
(defins 8
  (case (logand lsb #xF)
    (#x0 ; 8XY0 : set VX to the value of VY
     (set-register p (logand msb #xF)
		   (v (ash lsb -4) p)))
    (#x1 ; 8XY1 : set VX to VX OR VY
     (set-register p (logand msb #xF)
		   (logior (v (ash lsb -4) p)
			   (v (logand msb #xF) p)))
     ; 8XY1, 8XY2 and 8XY3 all clear VF when in chip8 mode.
     (if (enabled-p :clearvf)
	 (set-register p #xF 0)))
    (#x2 ; 8XY2 : set VX to VX AND VY
     (set-register p (logand msb #xF)
		   (logand (v (ash lsb -4) p)
			   (v (logand msb #xF) p)))
     (if (enabled-p :clearvf)
	 (set-register p #xF 0)))
    (#x3 ; 8XY3 : set VX to VX XOR VY
     (set-register p (logand msb #xF)
		   (logxor (v (ash lsb -4) p)
			   (v (logand msb #xF) p)))
     (if (enabled-p :clearvf)
	 (set-register p #xF 0)))
    (#x4 ; 8XY4 : set VX to VX + VY (set carry bit accordingly)
     (let ((total (+ (v (ash lsb -4) p)
		     (v (logand msb #xF) p))))
       (if (> total #xFF)
	   (set-register p #xF 1)
	   (set-register p #xF 0))
       (set-register p (logand msb #xF) (mod total #x100))))
    (#x5 ; 8XY5 : set VX to VX - VY, set VF to (NOT borrowed)
     (let ((res (- (v (logand msb #xF) p)
		   (v (ash lsb -4) p))))
       (if (< res 0)
	   (set-register p #xF 0)
	   (set-register p #xF 1))
       (set-register p (logand msb #xF) (mod res #x100))))
    (#x6 ; 8XY6 : shift VY right by one bit, set VX to result, store the shifted bit in VF
     ; If quirk set, shift VX instead of VY (VY gets completely ignored.)
     (let* ((x (logand msb #xF))
	    (y (ash lsb -4))
	    (flag-val (logand (v (if (enabled-p :shift) x y) p) 1)))
       (set-register p x
		     (ash (v (if (enabled-p :shift) x y) p) -1))
       (set-register p #xF flag-val)))
    (#x7 ; 8XY7 : set VX to VY - VX, set VF to (NOT borrowed)
     (let ((res (- (v (ash lsb -4) p)
		   (v (logand msb #xF) p))))
       (set-register p #xF
		     (if (< res 0) 0 1))
       (set-register p (logand msb #xF) (mod res #x100))))
    (#xE ; 8XYE : shift VY left by one bit, set VX to result, store shifted bit in VF
     ; Shift VX instead if :shift quirk is enabled.
     (let* ((x (logand msb #xF))
	    (y (ash lsb -4))
	    (flag-val (logand (v (if (enabled-p :shift) x y) p) #x80)))
       (set-register p x
		     (mod (ash (v (if (enabled-p :shift) x y) p) 1) #x100))
       (set-register p #xF (if (= flag-val 0) 0 1))))
    (t (error 'invalid-instruction
	      :ins full-ins
	      :program p
	      :text "Invalid N for 8XYN instruction.")))
  (inc-pc p))

; 9XY0 : Skip next instruction if VX != VY
(defins 9
  (unless (= (logand lsb #xF) 0)
    (error 'invalid-instruction :text "Unknown instruction." :ins full-ins :program p))
  (unless (= (v (logand msb #xF) p) (v (ash lsb -4) p))
    (inc-pc p))
  (inc-pc p))

; ANNN : set I to the address NNN
(defins 10
  (setf (program-i p) (logior (ash (logand msb #xF) 8) lsb))
  (inc-pc p))

; BNNN : set PC to (or jump to) NNN + V0
(defins 11
  (setf (program-pc p)
	     (+ (v 0 p)
		(logior (ash (logand msb #xF) 8) lsb))))

; CXNN : Set VX to random-number & NN
(defins 12
  (set-register p (logand msb #xF) (logand (random #x100) lsb))
  (inc-pc p))

(defmacro xorf (form1 form2)
  "Sets form1 to form1 ^ form2 via setf."
  `(setf ,form1 (logxor ,form1 ,form2)))

; DXYN - god... This is a MAJOR hack. I need to fix this.
; Abondon all hope, ye who enter here!..
(defins 13
  (let ((x (mod (v (logand msb #xF) p) #x40))
	(y (mod (v (ash lsb -4) p) #x20))
	(i (program-i p))
	(n (logand lsb #xF))
	(scn (program-screen p)))
    (loop while (> n 0) do
      (loop for j from 0 to 7 do
	(unless (or (> (+ x (- 7 j)) #x3F) (> y #x1F))
	  ; okay, so this whole mental gymnastics is happening because of a chip8 quirk.
	  ; the quirk in question requires us to set VF to 1 if and only if we set a 1 pixel to 0.
	  ; I'm sure there's a better way to do this, but screw that, this could've been just a single
	  ; line if it weren't for that damn quirk. Also, this will need to be rewritten
	  ; since the quirk needs to be toggleable. Also, we need to clip the damn sprite as well, and thats another quirk.
	  (let ((tmp (aref scn y (+ x (- 7 j)))))
	    (if (and (= 0
			(xorf (aref scn y (+ x (- 7 j)))
			      (logand (ash (aref (program-mem p) i) (- j)) 1)))
		     (= tmp 1))
		(set-register p #xF 1)))))
      (decf n)
      (incf i)
      (incf y))
    (inc-pc p)))

; E*** : TODO
(defins 14
  (if (> (v (logand msb #xF) p) #xF)
      (error 'invalid-instruction :text "Given Key number is bigger than F."
				  :program p :ins full-ins))
  (case lsb
    (#x9E
     (if (nth (v (logand msb #xF) p) *key-states*)
	 (inc-pc p))
     (inc-pc p))
    (#xA1
     (unless (nth (v (logand msb #xF) p) *key-states*)
       (inc-pc p))
     (inc-pc p))
    (t (error 'invalid-instruction :text "Unknown instruction." :program p :ins full-ins))))

; F*** 
(defins 15
  (case lsb
    (#x07 ; FX07 : sets VX to the value of DT
     (set-register p (logand msb #xF) (program-dt p))
     (inc-pc p))
    (#x0A ; FX0A : wait for key press, set VX to key
     ; Our current implementation checks if last-pressed-key is set.
     ; when a key is pressed last-pressed-key will be set by the main loop, and we can continue.
     (unless (null (program-last-pressed-key p))
       (set-register p (logand msb #xF) (program-last-pressed-key p))
       (inc-pc p)
       (setf (program-last-pressed-key p) nil)))
    (#x15 ; FX15 : set DT = VX
     (setf (program-dt p) (v (logand msb #xF) p))
     (inc-pc p))
    (#x18 ; FX18 : set ST = VX
     (setf (program-st p) (v (logand msb #xF) p))
     (inc-pc p))
    (#x1E ; FX1E : set I = I + VX
     (setf (program-i p)
	   (+ (program-i p)
	      (v (logand msb #xF) p)))
     (inc-pc p))
    (#x29 ; FX29 : set I = hex digit stored in VX. TODO: test.
     (if (> (v (logand msb #xF) p) #xF)
	 (error 'invalid-instruction :text "Argument to FX29 bigger than F" :program p :ins full-ins))
     (setf (program-i p)
	   (* 5 (v (logand msb #xF) p)))
     (inc-pc p))
    (#x33 ; FX33 : store the Binary coded decimal representation of VX in I, I + 1, I + 2
     (let ((mem (program-mem p))
	   (i (program-i p))
	   (vx (v (logand msb #xF) p)))
       (setf (aref mem i) (floor (/ vx 100)))
       (setf (aref mem (+ i 1))
	     (floor (mod (/ vx 10) 10)))
       (setf (aref mem (+ i 2))
	     (mod vx 10))
       (inc-pc p)))
    (#x55 ; FX55 : store V0-VX (inc) at I TODO: s-chip compatability here
     (dotimes (n (+ 1 (logand msb #xF)))
       (setf (aref (program-mem p)
		   (+ n (program-i p)))
	     (v n p)))
     (incf (program-i p)
	   (+ 1 (logand msb #xF)))
     (inc-pc p))
    (#x65 ; FX65 : load V0-VX (inc) from I TODO: s-chip compatability
     (dotimes (n (+ 1 (logand msb #xF)))
       (set-register p n
		     (aref (program-mem p)
			   (+ n (program-i p)))))
     (incf (program-i p)
	   (+ 1 (logand msb #xF)))
     (inc-pc p))))

; I put this stuff here because this function is essentially a
; huge table of instructions.
(defun run-instruction (p &optional (dbg-mode nil))
  "Runs the instruction at PC and increments PC accordingly."
  (let* ((msb (aref (program-mem p) (program-pc p)))
 	 (lsb (aref (program-mem p) (+ 1 (program-pc p))))
	 (full-ins (logior (ash msb 8) lsb)))
    (if dbg-mode
	(format *standard-output* "Executing ~4,'0X at ~4,'0X~%" full-ins (program-pc p)))
    (if (member (program-pc p) (program-brks p))
	(signal 'brk-cond :addr (program-pc p) :program p))
    ; The most significant nibble generally determines the instruction.
    (case (ash msb -4)
      (#x0 (ins-0 p full-ins))
      (#x1 (ins-1 p full-ins))
      (#x2 (ins-2 p full-ins))
      (#x3 (ins-3 p full-ins))
      (#x4 (ins-4 p full-ins))
      (#x5 (ins-5 p full-ins))
      (#x6 (ins-6 p full-ins))
      (#x7 (ins-7 p full-ins))
      (#x8 (ins-8 p full-ins))
      (#x9 (ins-9 p full-ins))
      (#xA (ins-10 p full-ins))
      (#xB (ins-11 p full-ins))
      (#xC (ins-12 p full-ins))
      (#xD (ins-13 p full-ins))
      (#xE (ins-14 p full-ins))
      (#xF (ins-15 p full-ins))
      ; Invalid instruction - error!
      (t (error 'invalid-instruction
		:ins full-ins :program p
		:text "Unknown/Invalid instruction")))))

; This shouldn't be used - it's only here for testing at the REPL.
(defun execute (p &optional (dbg-mode nil))
  (handler-case
    (loop
      (run-instruction p dbg-mode))
    (invalid-instruction (err)
      (format t "Error: ~a~%At position: ~3,'0X ~%Instruction: ~4,'0X" (text err) (program-pc p) (ins err)))
    (brk-cond (c)
      ; I'm still not quite sure if breakpoints are going to be a proper feature.
      (format t "Breakpoint hit.")
      (print c))))
