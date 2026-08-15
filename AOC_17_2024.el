;;; AOC_17_2024.el --- ULCE on AOC 2024 Day 17 -*- lexical-binding: t; -*-

;;; Commentary:
;; ULCE (Useless Lisp CPU Emulator)
;; Lisp version of day 17 from 2024.
;; Another take on solving this problem, goal is to sketch a more extendable cpu structure.

;;; Code:

(require 'cl-lib)

(defvar ulce-memsize 20
  "Default memory size.")
(setq ulce-memsize 20)

(cl-defstruct ulce-registers
  "Register structure."
  (a 0) (b 0) (c 0)
  )

(cl-defstruct ulce-machine
  "CPU structure, r: registers sub-structure, pc: program counter, mem: memory."
  (r-abc (make-ulce-registers)) (pc 0) (mem (make-vector ulce-memsize 0)))

(defvar ulce-cpu nil "Default global CPU structure.")
(setq ulce-cpu (make-ulce-machine))

(defun ulce--get-op (cpu pc)
  "Return current op-code from CPU at current PC."
  (aref (ulce-machine-mem cpu) pc))

(defun ulce--get-operand (cpu pc)
  "Return current operand from CPU at current PC."
  (aref (ulce-machine-mem cpu) (1+ pc)))

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
(defun ulce--exec-op (op operand cpu reg)
  "Execute OP with OPERAND on CPU and REG."
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
     (error "Bad opcode: %S" op))))

(defun ulce-run (cpu)
  "Run program on machine CPU until end of program."
  (let ((cpu (or cpu ulce-cpu)))
    (while  (< (ulce-machine-pc cpu) (length (ulce-machine-mem cpu)))
      (let* ((pc (ulce-machine-pc cpu))
             (op (ulce--get-op cpu pc))
             (operand (ulce--get-operand cpu pc))
             (reg (ulce-machine-r-abc cpu)))
        (cl-incf (ulce-machine-pc cpu) 2) ; advance pc always to for this cpu
        (ulce--exec-op op operand cpu reg)))))

(defun ulce-load-program  (prog &optional a b c cpu)
  "Load PROG vector into mem of CPU (defaults `ulce-cpu'), set A B C registers."
  (let ((cpu (or cpu ulce-cpu))
        (a (or a 0))
        (b (or b 0))
        (c (or c 0)))
    (setf (ulce-registers-a (ulce-machine-r-abc cpu)) a)
    (setf (ulce-registers-b (ulce-machine-r-abc cpu)) b)
    (setf (ulce-registers-c (ulce-machine-r-abc cpu)) c)
    (cl-replace (ulce-machine-mem cpu) prog)))

(defun testit (&optional pc cpu)
  "Run current program on CPU (default is the global `ulce-cpu') starting from PC."
  (let ((cpu (or cpu ulce-cpu))
        (pc (or pc 0 )))
    (setf (ulce-machine-pc cpu) pc)
    (ulce-run cpu)))

;; run test on the global default ulce-cpu
(ulce-load-program (vconcat [2 4 1 3 7 5 4 1 1 3 0 3 5 5 3 0]) 37283687 0 0)
(testit)
;(pp ulce-cpu)

;;; AOC_17_2024.el ends here
