(require :sdl2)
(require :mixalot)
(require :mixalot-mp3)
(defpackage :cl-chip8
  (:use
   :cl
   :sdl2 ; we use SDL2 for display
   )
  (:export
   :program
   :make-program
   :program-screen
   :program-mem
   :program-regs
   :program-i
   :program-pc
   :program-stack
   :read-program

   :v ;to access registers
   :run-instruction
   :execute
   :memdump
   :regdump

					; conditions
   :invalid-instruction
   :brk-cond))
