;;; cavy-mode.el --- Cavy development environment -*-lexical-binding: t-*-

;; Copyright (C) 2020-2020 mcncm

;; Version: 0.1
;; Author: mcncm
;; Maintianer: mcncm
;; Created: 2 Dec 2020

;; Keywords: languages

;; This file is not part of GNU Emacs.

;; This file is distributed under the terms of both the mit license and the
;; apache license (version 2.0).

;;; Commentary:

;; This file is under active development and is likely to change rapidly.

;;; Code:

;;;;;;;;;;;;;;;;;
;; Compilation ;;
;;;;;;;;;;;;;;;;;

(defcustom cavy-compile-target 'qasm
  "What should be the output of the Cavy compiler?"
  :type 'symbol
  :group 'cavy
  :safe #'symbolp)

(defcustom cavy-latex-compile-method 'native
  "What LaTeX backend should be used?"
  :type 'symbol
  :group 'cavy
  :safe #'symbolp)

(defcustom cavy-preview-buffer "cavy-preview"
  "Name of the buffer to write Cavy compile output to."
  :type 'string
  :group 'cavy
  :safe #'stringp)

(defcustom cavy-binary "cavy"
  "Name of the Cavy binary to call."
  :type 'string
  :group 'cavy
  :safe #'stringp)

(defcustom cavy-opt-level 0
  "Optimization level for cavy compiler."
  :type 'integer
  :group 'cavy
  :safe #'integerp)

(defcustom cavy-comptime 't
  "Do constant propagation optimization."
  :type 'boolean
  :group 'cavy
  :safe #'booleanp)

(defcustom cavy-phase 'nil
  "Compilation phase to stop at. if NIL, proceed to code generation."
  :group 'cavy)

(defcustom cavy-debug 't
  "Run compiler in debug mode if not NIL."
  :type 'boolean
  :group 'cavy
  :safe #'booleanp)

(defcustom cavy-target 'nil
  "Compile target for Cavy code."
  :type 'string
  :group 'cavy
  :safe #'stringp)

(defcustom cavy-options '()
  "Options passed to the Cavy binary."
  :type 'list
  :group 'cavy
  :safe #'listp)

(defun cavy-compile-command ()
  "Genearte the Cavy compilation command."
  (let ((target (if cavy-target
                    `("--target" ,cavy-target)
                  'nil))
        (phase (if cavy-phase
                   `("--phase" ,cavy-phase)
                 'nil))
        (opt (if cavy-opt-level
                 `("-O" ,(number-to-string cavy-opt-level))
               'nil))
        (comptime (if cavy-comptime
                      'nil
                    '("--no-comptime")))
        (dbg (if cavy-debug
                 '("--debug")
               'nil))
        (in "/dev/stdin")
        (out "/dev/stdout")
        )
    `(,cavy-binary ,in "-o" ,out ,@opt ,@target ,@comptime ,@phase ,@dbg)))

(defun cavy-native-latex-compile-command ()
  "Command to run when compiling to latex."
  (let* ((out-dir "DIR")
         (tex-doc "cavy.tex")
         (pdf-doc "cavy.pdf")
         (shell-script
          (concat out-dir "=`mktemp -d`; "
                  ;; No latex file polution, please!
                  "cd $" out-dir "; "
                  ;; Now make the .tex document
                  cavy-binary
                  " /dev/stdin -o " tex-doc " --typecheck --target latex --debug; "
                  ;; And compile it! Note that latexmk is very noisy, and we
                  ;; have to supress all its output in order for us to get a
                  ;; well-formed pdf out of this.
                  "latexmk " tex-doc " -pdf > /dev/null 2>&1; "
                  ;; Emit the pdf, but supress error message if it's not there.
                  "cat " pdf-doc " 2> /dev/null")))
    `("bash" "-c" ,shell-script)))

(defun cavy-compile-buffer-setup (process buffer)
  "Command to ready BUFFER used by PROCESS before receiving compilation output."
  (cond ((eq 'latex cavy-compile-target)
         (cavy-compile-buffer-setup-latex process buffer))))

(defun cavy-compile-buffer-setup-latex (process buffer)
  "Prepare a BUFFER to view PDFs generted by PROCESS."
  (with-current-buffer (get-buffer buffer)
    (if
        (eq 0 (process-exit-status process))
        (progn
          ;; Default encoding is UTF-8, which doesn't accept some bytes in the
          ;; PDF headers.
          (set-buffer-file-coding-system 'raw-text)
          (pdf-view-mode))
        (progn
          ;; Something has gone wrong--presumably the program failed to compile.
          ;; Display the error message. I don’t know why the error message has
          ;; scrolled off screen, but let’s go back up in order to see it.
          (goto-char (point-min))
          ;; `special-mode' is the major mode used in my `*Messages*' buffer, so
          ;; it seems reasonable. Is there a better choice?
          (special-mode)))))

(defun cavy-compile-process (buffer)
  "Make a Cavy compilation process in buffer BUFFER."
  (make-process :name "cavy-process" :buffer buffer :command (cavy-compile-command)))

(defun cavy-compile-program (buffer program)
  "Compile a PROGRAM and return the object code.
The BUFFER argument is the buffer to write output to."
  (let ((process (cavy-compile-process buffer)))
    (process-send-string process program)
    (process-send-eof process)
    (accept-process-output process)
    (cavy-compile-buffer-setup process buffer)))

(defun cavy-compile-and-preview ()
  "Recompile and display output in a temporary buffer."
  (interactive)
  (with-output-to-temp-buffer cavy-preview-buffer
    (cavy-compile-program cavy-preview-buffer (buffer-string))))

(defun cavy-after-save-hook ()
  "Action to take after saving the cavy file."
  (cavy-compile-and-preview))

;;;;;;;;;;;;
;; Syntax ;;
;;;;;;;;;;;;

(defcustom cavy-indent-offset 4
  "Indent Cavy code by this many spaces."
  :type 'integer
  :group 'cavy
  :safe #'integerp)

;; `define-derived-mode' will find this name automatically and use this table.
(defvar cavy-mode-syntax-table nil
  "Syntax table for `cavy-mode'.")

(setq cavy-mode-syntax-table
      (let ((syn-table (make-syntax-table)))
        (modify-syntax-entry ?\/ ". 12b" syn-table)
        (modify-syntax-entry ?\n "> b" syn-table)
        syn-table))

(defconst cavy-keywords
  '("if" "else" "for" "in"
    "as" "let" "fn" "print"
    "struct" "enum" "type"
    "true" "false")
  "Cavy keywords for font-locking.")

(defconst cavy-types
  '("bool" "u8" "u16" "u32")
  "Cavy's built-in base types.")

(defconst cavy-builtins
  '("flip" "split"
    "len" "enumerate" "zip"
    "qalloc" "free"))

(defface cavy-special-operators-face
  '((t :weight bold :inherit font-lock-builtin-face))
  "Face for the linearization and of course operators."
  :group 'cavy)

(defvar cavy-font-lock-keywords
  `(
    (,(regexp-opt cavy-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt cavy-types 'symbols) . font-lock-type-face)
    (,(regexp-opt cavy-builtins 'symbols) . font-lock-builtin-face)
    ("\\?" . 'cavy-special-operators-face)
    ("\\!" . 'cavy-special-operators-face)
    )
  "Font-lock definitions.")

;;;;;;;;;;;;;;;;;;;;;
;; Mode definition ;;
;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-derived-mode cavy-mode prog-mode "Cavy"
  "Major mode for editing Cavylang code."

  ;; Fontification
  (setq-local font-lock-defaults '((cavy-font-lock-keywords)))

  ;; Preview compiled code
  (add-hook 'after-save-hook 'cavy-after-save-hook nil t))

(provide 'cavy-mode)

;;; cavy-mode.el ends here
