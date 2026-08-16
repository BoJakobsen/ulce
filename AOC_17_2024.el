;;; AOC_17_2024.el --- ULCE on AOC 2024 Day 17 -*- lexical-binding: t; -*-

;;; Commentary:
;; ULCE (Useless Lisp CPU Emulator)
;; Lisp version of day 17 from 2024.
;; Another take on solving this problem, goal is to sketch a more extendable cpu structure.

;;; Code:

(require 'cl-lib)

(defvar ulce-memsize 20
  "Default memory size.")
(setq ulce-memsize 20) ; set here for applying change on re-eval of buffer

(cl-defstruct ulce-registers
  "Register structure."
  (a 0) (b 0) (c 0)
  )

(cl-defstruct ulce-machine
  "CPU structure, r: registers sub-structure, pc: program counter, mem: memory, stop-code."
  (r-abc (make-ulce-registers)) (pc 0) (mem (make-vector ulce-memsize 0)) (stop-code 99) )

(defvar ulce-cpu nil "Default global CPU structure.")
(setq ulce-cpu (make-ulce-machine)); set here for applying change on re-eval of buffer

(defun ulce--mem-read (cpu addr)
  "Retuns contents of memory address ADDR from CPU."
  (aref (ulce-machine-mem cpu) addr))

(defun ulce--advance (cpu)
  "Retuns contents from CPU memory address pc, and advance pc to next byte."
 (prog1
  (ulce--mem-read cpu (ulce-machine-pc cpu))
  (cl-incf (ulce-machine-pc cpu) 1)))

(defun ulce--decode-combo-operand (operand reg)
  "Return current combo-operand for OPERAND using register data REG."
  (pcase operand
    (4
     (ulce-registers-a reg))
    (5
     (ulce-registers-b reg))
    (6
     (ulce-registers-c reg))
    (7
     (error "Combo operand 7, is not in use"))
    (operand ; operand 0 -- 3 are passed through
     operand)))

;; Version using bit-vise operations, deduced from the original puzzle.
(defun ulce--exec-op (cpu op operand)
  "Execute OP with OPERAND on CPU."
  (let ((reg (ulce-machine-r-abc cpu)))
    (pcase op
      (0
       (setf (ulce-registers-a reg)  (ash (ulce-registers-a reg) (- (ulce--decode-combo-operand operand reg)))))
      (1
       (setf (ulce-registers-b reg)  (logxor (ulce-registers-b reg)  operand)))
      (2
       (setf (ulce-registers-b reg)  (logand (ulce--decode-combo-operand operand reg) 7 )))
      ((and 3 (guard (not (zerop (ulce-registers-a reg)))))
       (setf (ulce-machine-pc cpu)  operand))
      (3
       nil) ; jnz not taken
      (4
       (setf (ulce-registers-b reg)  (logxor (ulce-registers-b reg)  (ulce-registers-c reg))))
      (5
       (message "%d" (logand (ulce--decode-combo-operand operand reg) 7)))
      (6
       (setf (ulce-registers-b reg)  (ash (ulce-registers-a reg) (- (ulce--decode-combo-operand operand reg)))))
      (7
       (setf (ulce-registers-c reg)  (ash (ulce-registers-a reg) (- (ulce--decode-combo-operand operand reg)))))
      (op ; matches all and binds to op
       (error "Bad opcode: %S" op)))))

(defun ulce-step (&optional cpu)
  "Step program on machine CPU. Return nil if program end reached."
  (let ((cpu (or cpu ulce-cpu)))
    (let*  ((op (ulce--advance cpu))
            (operand (ulce--advance cpu))) ; operand is always second byte
      (unless (= op (ulce-machine-stop-code cpu))
        (ulce--exec-op cpu op operand)
        t))))

(defun ulce-run (&optional cpu pc)
  "Run program on machine CPU until end of program, optional from PC."
  (let ((cpu (or cpu ulce-cpu)))
    (if pc (setf (ulce-machine-pc cpu) pc))
    (while (ulce-step cpu))))

(cl-defun ulce-load-program  (prog &key (cpu ulce-cpu) (a 0) (b 0) (c 0))
  "Load PROG vector into mem of CPU (defaults `ulce-cpu'), set A B C registers, add stop code."
  (let ((cpu (or cpu ulce-cpu))
        (a (or a 0))
        (b (or b 0))
        (c (or c 0)))
    (setf (ulce-registers-a (ulce-machine-r-abc cpu)) a)
    (setf (ulce-registers-b (ulce-machine-r-abc cpu)) b)
    (setf (ulce-registers-c (ulce-machine-r-abc cpu)) c)
    (cl-replace (ulce-machine-mem cpu) (vconcat prog (list (ulce-machine-stop-code cpu))))))

(defun testit-AOC17 (&optional cpu)
  "Load and run a test program on CPU (default is the global `ulce-cpu')."
  (let ((cpu (or cpu ulce-cpu)))
    (ulce-load-program [2 4 1 3 7 5 4 1 1 3 0 3 5 5 3 0] :cpu cpu :a 37283687 :b 0 :c 0)
    (ulce-run cpu 0)))

;; run test on the global default ulce-cpu
(testit-AOC17)
;(pp ulce-cpu)

;;; AOC_17_2024.el ends here
