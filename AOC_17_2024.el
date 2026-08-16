;;; AOC_17_2024.el --- ULCE on AOC 2024 Day 17 -*- lexical-binding: t; -*-

;;; Commentary:
;; ULCE (Useless Lisp CPU Emulator)
;; Lisp version of day 17 from 2024.
;; Another take on solving this problem, goal is to sketch a more extendable cpu structure.

;;; Code:

(require 'cl-lib)

(defvar ulce-memsize 20
  "Memory size.")
(setq ulce-memsize 20) ; set here for applying change on re-eval of buffer

(cl-defstruct ulce-registers
  "Register structure."
  (a 0) (b 0) (c 0)
  )

(cl-defstruct ulce-machine
  "CPU structure, r: registers sub-structure, pc: program counter, mem: memory, stop-code."
  (r-abc (make-ulce-registers)) (pc 0) (mem (make-vector ulce-memsize 0)) (stop-code 99))

(defun ulce--define-op (n)
  "Primitively generate op code table of N entries (to be replaced with macros)."
  (let ((op (make-vector n nil)))
    (aset op 0
          (lambda (_cpu reg operand)
            (setf (ulce-registers-a reg)  (ash (ulce-registers-a reg) (- (ulce--decode-combo-operand operand reg))))))
    (aset op 1
          (lambda (_cpu reg operand)
            (setf (ulce-registers-b reg)  (logxor (ulce-registers-b reg)  operand))))
    (aset op 2
          (lambda (_cpu reg operand)
            (setf (ulce-registers-b reg)  (logand (ulce--decode-combo-operand operand reg) 7 ))))
    (aset op 3
          (lambda (cpu reg operand)
            (if (not (zerop (ulce-registers-a reg)))  (setf (ulce-machine-pc cpu)  operand)
              nil)))
    (aset op 4
          (lambda (_cpu reg _operand)
            (setf (ulce-registers-b reg)  (logxor (ulce-registers-b reg)  (ulce-registers-c reg)))))
    (aset op 5
          (lambda (_cpu reg operand)
            (message "%d" (logand (ulce--decode-combo-operand operand reg) 7))))
    (aset op 6
          (lambda (_cpu reg operand)
            (setf (ulce-registers-b reg)  (ash (ulce-registers-a reg) (- (ulce--decode-combo-operand operand reg))))))
    (aset op 7
          (lambda (_cpu reg operand)
            (setf (ulce-registers-c reg)  (ash (ulce-registers-a reg) (- (ulce--decode-combo-operand operand reg))))))
    op))

(defvar ulce-opcodes nil
  "Global opcode table.")
(setq ulce-opcodes (ulce--define-op 8))

(defvar ulce-cpu nil "Default global CPU structure.")
(setq ulce-cpu (make-ulce-machine)); 

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

(defun ulce--exec-op (cpu op operand opcodes)
  "Execute OP with OPERAND on CPU."
  (let ((reg (ulce-machine-r-abc cpu)))
    (funcall (aref opcodes op) cpu reg operand)))
;; some check for bad op would be good

(defun ulce-step (&optional cpu)
  "Step program on machine CPU. Return nil if program end reached."
  (let* ((cpu (or cpu ulce-cpu))
         (opcodes ulce-opcodes)
         (op (ulce--advance cpu))
         (operand (ulce--advance cpu))) ; operand is second byte
         (unless (= op (ulce-machine-stop-code cpu))
           (ulce--exec-op cpu op operand opcodes)
           t)))

(defun ulce-run (&optional cpu pc)
  "Run program on machine CPU until end of program, optional from PC."
  (let ((cpu (or cpu ulce-cpu)))
    (when pc (setf (ulce-machine-pc cpu) pc))
    (while (ulce-step cpu))))

(cl-defun ulce-load-program  (program &key (cpu ulce-cpu) (a 0) (b 0) (c 0))
  "Load PROGRAM into mem of CPU, set A B C registers, add stop code."
  (if (> (+ 2 (length program)) ulce-memsize) ; space for stop code needed
      (error "Program is to long")
    (setf (ulce-registers-a (ulce-machine-r-abc cpu)) a)
    (setf (ulce-registers-b (ulce-machine-r-abc cpu)) b)
    (setf (ulce-registers-c (ulce-machine-r-abc cpu)) c)
    (cl-replace (ulce-machine-mem cpu) (vconcat program (list (ulce-machine-stop-code cpu))))))

(defun testit-AOC17 (&optional cpu)
  "Load and run a test program on CPU (default is the global `ulce-cpu')."
  (let ((cpu (or cpu ulce-cpu)))
    (ulce-load-program [2 4 1 3 7 5 4 1 1 3 0 3 5 5 3 0] :cpu cpu :a 37283687 :b 0 :c 0)
    (ulce-run cpu 0)))

;; run test on the global default ulce-cpu
(testit-AOC17)
;(pp ulce-cpu)

;;; AOC_17_2024.el ends here
