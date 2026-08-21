;;; AOC_17_2024.el --- ULCE on AOC 2024 Day 17 -*- lexical-binding: t; -*-

;;; Commentary:
;; ULCE (Useless Lisp CPU Emulator)
;; Lisp version of day 17 from 2024.
;; Another take on solving this problem, goal is to sketch a more extendable VM structure.

;; Very simple (and generally useless) VM from AOC 17 2024.
;; Memory layout for this VM is integer representation with each block being {op, operand} 
;; Registers are a,b,c general purpose 
;; 8 opcodes (0 -> 7) inc jnz (op #3) and print (op #5)
;; op #99 added to this VM for halt

;; The AOC problem part 1 provides a program/data list, and a initial value for reg a
;; solution is the output when the program halts (reaches the end of program)

;;; Code:

(require 'cl-lib)

;; VM setup
(cl-defstruct ulce-machine
  "VM structure, r: registers sub-structure, pc: program counter, mem: memory, stop-code."
  (r-abc (make-ulce-registers)) (pc 0) (mem (make-vector ulce-memsize 0)) (stop-code 99))

(cl-defstruct ulce-registers
  "Register structure."
  (a 0) (b 0) (c 0)
  )

(defmacro ulce--decode (vm acc &optional operand)
  "Register access, return contents of REG (a,b,c) from VM."
  (pcase acc
    ('a
     `(ulce-registers-a (ulce-machine-r-abc ,vm)))
    ('b
     `(ulce-registers-b (ulce-machine-r-abc ,vm)))
    ('c
     `(ulce-registers-c (ulce-machine-r-abc ,vm)))
    ('combo
     `(ulce--decode-combo-operand ,vm ,operand))
    ('oper
     `,operand)))

;; for debug
;(macroexpand-1 '(ulce--decode vm combo a))

(defun ulce--decode-combo-operand (vm operand)
  "Return current combo-operand for OPERAND using VM."
  (pcase operand
    (4
     (ulce--decode vm a))
    (5
     (ulce--decode vm b))
    (6
     (ulce--decode vm c))
    (7
     (error "Combo operand 7, is not in use"))
    (operand ; operand 0 -- 3 are passed through
     operand)))

(defun ulce--mem-read (vm addr)
  "Retuns contents of memory address ADDR from VM."
  (aref (ulce-machine-mem vm) addr))

(cl-defmacro ulce--set-op (op-vec op-code &key out opr1 opr2 expr)
  "Define OP-CODE in vector OP-VEC. EXPR is in terms of opr1 and opr2"
  `(aset ,op-vec ,op-code
         (lambda (vm operand)
           (let ((opr1 (ulce--decode vm ,opr1 operand))
                 (opr2 (ulce--decode vm ,opr2 operand)))
             (setf (ulce--decode vm ,out) ,expr)))))

;; (ulce--define-op 8)
;;(ulce--decode ulce-vm oper 1)

(defun ulce--define-op (n)
  "Primitively generate op code table of N entries (to be replaced with macros)."
  (let ((op (make-vector n nil)))
    (ulce--set-op op 0
                  :out a
                  :opr1 a
                  :opr2 combo
                  :expr (ash opr1 (- opr2)))
    (aset op 1
          (lambda (vm operand)
            (setf (ulce--decode vm b)  (logxor (ulce--decode vm b)  (ulce--decode vm oper operand)))))
    (aset op 2
          (lambda (vm operand)
            (setf (ulce--decode vm b)  (logand (ulce--decode vm combo operand) 7 ))))
    (aset op 3
          (lambda (vm operand)
            (if (not (zerop (ulce--decode vm a)))  (setf (ulce-machine-pc vm)  operand)
              nil)))
    (aset op 4
          (lambda (vm _operand)
            (setf (ulce--decode vm b)  (logxor (ulce--decode vm b)  (ulce--decode vm c)))))
    (aset op 5
          (lambda (vm operand)
            (message "%d" (logand (ulce--decode vm combo operand) 7))))
    (aset op 6
          (lambda (vm operand)
            (setf (ulce--decode vm b)  (ash (ulce--decode vm a) (- (ulce--decode vm combo operand))))))
    (aset op 7
          (lambda (vm operand)
            (setf (ulce--decode vm c)  (ash (ulce--decode vm a) (- (ulce--decode vm combo operand))))))
    op))


;; Define machine running functions

(defun ulce--advance (vm)
  "Retuns contents from VM memory address pc, and advance pc to next byte."
 (prog1
  (ulce--mem-read vm (ulce-machine-pc vm))
  (cl-incf (ulce-machine-pc vm) 1)))

;; some check for bad op would be good
(defun ulce--exec-op (vm op operand opcodes)
  "Execute OP with OPERAND on VM."
    (funcall (aref opcodes op) vm operand))


(defun ulce-step (&optional vm)
  "Step program on machine VM. Return nil if program end reached."
  (let* ((vm (or vm ulce-vm))
         (opcodes ulce-opcodes)
         (op (ulce--advance vm))
         (operand (ulce--advance vm))) ; operand is second byte
         (unless (= op (ulce-machine-stop-code vm))
           (ulce--exec-op vm op operand opcodes)
           t)))

(defun ulce-run (&optional vm pc)
  "Run program on machine VM until end of program, optional start at PC."
  (let ((vm (or vm ulce-vm)))
    (when pc (setf (ulce-machine-pc vm) pc))
    (while (ulce-step vm))))

;; IO and loader functions
(cl-defun ulce-load-program  (program &key (vm ulce-vm) (a 0) (b 0) (c 0))
  "Load PROGRAM into mem of VM, set A B C registers, add stop code."
  (if (> (+ 2 (length program)) ulce-memsize) ; space for stop code needed
      (error "Program is to long")
    (setf (ulce--decode vm a) a)
    (setf (ulce--decode vm b) b)
    (setf (ulce--decode vm c) c)
    (cl-replace (ulce-machine-mem vm) (vconcat program (list (ulce-machine-stop-code vm))))))

;; Define global VM and Op-code tabel
(defvar ulce-memsize 20
  "Memory size.")
(setq ulce-memsize 20) ; set here for applying change on re-eval of buffer

(defvar ulce-opcodes nil
  "Global opcode table.")
(setq ulce-opcodes (ulce--define-op 8))

(defvar ulce-vm nil "Default global default VM.")
(setq ulce-vm (make-ulce-machine)); 

;; Define AOC 17 2024 program, and tester function
(defun testit-AOC17 (&optional vm)
  "Load and run a test program on VM (default is the global `ulce-vm')."
  (let ((vm (or vm ulce-vm)))
    (ulce-load-program [2 4 1 3 7 5 4 1 1 3 0 3 5 5 3 0] :vm vm :a 37283687 :b 0 :c 0)
    (ulce-run vm 0)))


(testit-AOC17)
;(pp ulce-vm)

;;; AOC_17_2024.el ends here
