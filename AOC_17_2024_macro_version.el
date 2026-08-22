;;; AOC_17_2024.el --- ULCE on AOC 2024 Day 17 -*- lexical-binding: t; -*-

;;; Commentary:
;; ULCE (Useless Lisp CPU Emulator)
;; Lisp version of AOC day 17 from 2024.
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
  (r-abc (make-ulce-registers)) (pc 0) (mem (make-vector ulce-memsize 0)) (stop-code 99)
  (print (lambda (val) (message "%s" val))))

(cl-defstruct ulce-registers
  "Register structure."
  (a 0) (b 0) (c 0)
  )

(defun ulce-print (vm val)
  "Print VAL using the print method of VM."
  (funcall (ulce-machine-print vm) val))

(defmacro ulce--decode (vm acc &optional operand)
    "Expand to a place or value form for part of VM, selected by ACC.

ACC chooses what is decoded:
  a / b / c -- the corresponding register (setf-able place)
  pc        -- the program counter (setf-able place)
  mem       -- OPERAND used as an address into VM's memory (setf-able place)
  combo     -- the AOC \"combo operand\" value of OPERAND (read-only)
  operand   -- OPERAND itself, unchanged (read-only)
  nil       -- explicit no-op, expands to nil

Any other ACC signals an error at expansion time."
  (pcase acc
    ('a
     `(ulce-registers-a (ulce-machine-r-abc ,vm)))
    ('b
     `(ulce-registers-b (ulce-machine-r-abc ,vm)))
    ('c
     `(ulce-registers-c (ulce-machine-r-abc ,vm)))
    ('combo
     `(ulce--decode-combo-operand ,vm ,operand))
    ('operand
     `,operand)
    ('pc
     `(ulce-machine-pc ,vm))
    ('mem
     `(aref (ulce-machine-mem ,vm) ,operand))
    ('nil nil)
    (_ (error "ulce--decode: Unknown accessor %S" acc))))

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

(cl-defmacro ulce--set-op (op-vec op-code &key out opr1 opr2 expr print side-effect)
  "Install an opcode handler for OP-CODE into vector OP-VEC.

OPR1 and OPR2 are ACC symbols (see `ulce--decode') decoded once per call
and bound to `opr1'/`opr2' for use inside EXPR.

OUT also an ACC symbol: EXPR's value is written there via
setf if OUT is not nil.

PRINT if set, triggers print out of EXPR through the VM's print facility.

SIDE-EFFECT can be any form using `opr1'/`opr2'/`expr'/`out'."
  `(aset ,op-vec ,op-code
         (lambda (vm operand)
           (let* ((opr1 (ulce--decode vm ,opr1 operand))
                  (opr2 (ulce--decode vm ,opr2 operand))
                  (expr ,expr))
             ,(when side-effect
                `(let ((out ',out))
                   ,side-effect))
             ,(when print
                `(ulce-print vm expr))
             ,(when out
                `(setf (ulce--decode vm ,out) ,expr))))))

(defun ulce--define-op (n)
  "Generate op code table of N entries."
  (let ((op (make-vector n nil)))
    (ulce--set-op op 0
                  :out a
                  :opr1 a
                  :opr2 combo
                  :expr (ash opr1 (- opr2))
                  :side-effect nil
                  )
    (ulce--set-op op 1
                  :out b
                  :opr1 b
                  :opr2 operand
                  :expr (logxor opr1 opr2))
    (ulce--set-op op 2
                  :out b
                  :opr1 combo
                  :opr2 nil
                  :expr (logand opr1 7))
    (ulce--set-op op 3
                  :out pc
                  :opr1 a
                  :opr2 pc
                  :expr (if (not (zerop opr1)) operand opr2))
    (ulce--set-op op 4
                  :out b
                  :opr1 b
                  :opr2 c
                  :expr (logxor opr1 opr2))
    (ulce--set-op op 5
                  :out nil
                  :opr1 combo
                  :opr2 nil
                  :expr (logand opr1 7)
                  :print t)
    (ulce--set-op op 6
                  :out b
                  :opr1 a
                  :opr2 combo
                  :expr (ash opr1 (- opr2)))
    (ulce--set-op op 7
                  :out c
                  :opr1 a
                  :opr2 combo
                  :expr (ash opr1 (- opr2)))
    op))


;; Define machine running functions

(defun ulce--advance (vm)
  "Retuns contents from VM memory address pc, and advance pc to next byte."
 (prog1
     (ulce--decode vm mem (ulce--decode vm pc))
  (cl-incf (ulce--decode vm pc) 1)))

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

(define-derived-mode ulce-console-mode fundamental-mode "ULCE-Console"
  "Major mode for the ULCE Wozmon-style console.")

(defun ulce-console-print (val)
  "Print VAL to \"ulce/console\" buffer"
  (with-current-buffer (get-buffer-create "ulce/console")
    (unless (derived-mode-p 'ulce-console-mode)
      (ulce-console-mode))
    (goto-char (point-max))
      (insert (format "%s" val))))

(defun ulce-console-execute-line ()
  "Read and execute last line on monitor buffer."
  (interactive)
  (let ((cmd (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (message "%s" cmd)
    (insert "\n\\")))

(keymap-set ulce-console-mode-map "RET" #'ulce-console-execute-line)

;; Define global VM and Op-code tabel
(defvar ulce-memsize 20
  "Memory size.")
(setq ulce-memsize 20) ; set here for applying change on re-eval of buffer

(defvar ulce-opcodes nil
  "Global opcode table.")
(setq ulce-opcodes (ulce--define-op 8))

(defvar ulce-vm nil "Default global default VM.")
(setq ulce-vm (make-ulce-machine :print (lambda (val) (ulce-console-print val)))); global vm using colsole

;; Define AOC 17 2024 program, and tester function
(defun testit-AOC17 (&optional vm)
  "Load and run a test program on VM (default is the global `ulce-vm')."
  (let ((vm (or vm ulce-vm)))
    (ulce-print vm "\ntestit-AOC17: \n" )
    (ulce-load-program [2 4 1 3 7 5 4 1 1 3 0 3 5 5 3 0] :vm vm :a 37283687 :b 0 :c 0)
    (ulce-run vm 0)
    (ulce-print vm "\n")))

(testit-AOC17)

;;; AOC_17_2024_macro_version.el ends here
