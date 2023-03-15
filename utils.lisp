(in-package :cl-chip8)

(defun memdump (p)
  "Dumps entire memory to stdout in human-readable format."
  (do ((i 0 (+ i #x10))
       (mem (program-mem p)))
      ((>= i (length mem)) nil)
    (format *standard-output* "~3,'0X: " i)
    (loop for j from 0 to #xF
	  do (format  *standard-output* "~2,'0X "
		      (aref mem (+ i j))))
    (format *standard-output* "~%"))
  (force-output))

(defun regdump (p)
  "Dumps the states of all registers in human-readable form."
  (format *standard-output* "PC: ~3,'0X~%" (program-pc p))
  (loop for i from 0 to 3 do
    (loop for j from 0 to 3 do
      (format *standard-output*
	      "V~X=0x~2,'0X~2t"
	      (+ (* i 4) j) (aref (program-regs p) (+ (* i 4) j))))
    (terpri *standard-output*))
  (format *standard-output*
	  "Call stack:~%~{~4t0x~4,'0X~%~}~%" (program-stack p))
  (format *standard-output*
	  "Delay Timer: ~2,'0X ~3t Sound Timer: ~2,'0X~%" (program-dt p) (program-st p)))
