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

(defcustom cavy-preview-buffer "cavy-preview"
  "Name of the buffer to write Cavy compile output to."
  :type 'string
  :group 'cavy
  :safe #'stringp)

(defcustom cavy-tempdir 'nil
  "Directory for temp files, e.g. for LaTeX compilation."
  :type 'string
  :group 'cavy
  :safe #'stringp)

(defcustom cavy-binary "cavyc"
  "Name of the Cavy binary to call."
  :type 'string
  :group 'cavy
  :safe #'stringp)

(defcustom cavy-options
  '((opt-level    . 3)
    (no-comptime  . nil)                ; no constant propagation
    (phase        . nil)
    (debug        . t)
    (feedback     . t)

    ;; target options
    (target       . nil)
    (standalone   . nil)                ; compile latex as standalone
    (package      . quantikz)           ; latex package
    (wave         . nil)                ; quantikz wave
    (initial-kets . t)                  ; use initial kets
    (perf         . t)                  ; report profiling statistics

    ;; measurement options
    (meas-mode    . demolition))

  "Options passed to the Cavy command line interface."
  :group 'cavy)

(defun cavy-get-opt (key)
  "Get the value of option `KEY'."
  (cdr (assq key cavy-options)))

(defun cavy-flag (flag key)
  "Get the value of option `FLAG' from `cavy-options' key `KEY'."
  (let* ((value (cavy-get-opt key)))
    (cond ((stringp value)
           `(,flag ,value))
          ((numberp value)
           `(,flag ,(number-to-string value)))
          ((eq value 't)
           `(,flag))
          ((not value)
           'nil))))

(defun cavy-set-opt (key value)
  "Set the value of option `KEY' to value `VALUE'."
  (interactive)
  (setf (alist-get key cavy-options) value))

;; How can this be done without a double-lookup?
(defun cavy-toggle-opt (key)
  "Toggle the value of option `KEY' between `t and nil."
  (interactive)
  "Toggle the value of option `KEY'."
  (let* ((entry (alist-get key cavy-options))
         (new-value (if entry 'nil 't)))
    (setf (alist-get key cavy-options) new-value)))

(defun cavy-cli-command (&optional in out)
  "Generate the compilation command for the Cavy CLI. Optionally takes input and output files `IN' and `OUT'."
  (let ((target (cavy-flag "--target" 'target))
        (package (cavy-flag "--package" 'package))
        (standalone (cavy-flag "--standalone" 'standalone))
        (kets (cavy-flag "--initial-kets" 'initial-kets))
        (wave (cavy-flag "--wave" 'wave))
        (perf (cavy-flag "--perf" 'perf))

        (phase (cavy-flag "--phase" 'phase))
        (opt (cavy-flag "-O" 'opt-level))
        (comptime (cavy-flag "--no-comptime" 'no-comptime))
        (dbg (cavy-flag "--debug" 'debug))
        (feedback (cavy-flag "--feedback" 'feedback))
        (meas-mode (cavy-flag "--meas-mode" 'meas-mode))
        ;; handle default values
        (in (or in "/dev/stdin"))
        (out (or out "/dev/stdout")))
        
    `(,cavy-binary ,in "-o" ,out
                   ,@opt ,@target ,@standalone ,@package ,@kets
                   ,@perf ,@comptime ,@phase ,@dbg ,@feedback
                   ,@meas-mode ,@wave)))

(defun cavy-make-or-get-tempdir ()
  "Get the tempdir used for building latex documents."
  (if (and cavy-tempdir (file-directory-p cavy-tempdir))
      cavy-tempdir
    (let ((tmp (make-temp-file "cavy" t)))
      (setq cavy-tempdir tmp))))

(defun cavy-build-pdf ()
  "Command to run when compiling to latex."
  (let* ((out-dir (cavy-make-or-get-tempdir))
         (tex-doc "cavy.tex")
         (pdf-doc "cavy.pdf")
         (shell-script
          (concat "cd " out-dir "; "
                  ;; Now make the .tex document
                  (mapconcat 'identity (cavy-cli-command 'nil tex-doc) " ") "; "
                  ;; And compile it! Note that pdflatex is very noisy, and we
                  ;; have to supress all its output in order for us to get a
                  ;; well-formed pdf out of this.
                  "pdflatex " tex-doc " -pdf > /dev/null 2>&1; "
                  ;; Emit the pdf, but supress error message if it's not there.
                  "cat " pdf-doc " 2> /dev/null")))
    `("bash" "-c" ,shell-script)))

(defun cavy-pdf-preview-p ()
  "Do the current option settings create a .pdf?"
  (and (cavy-get-opt 'standalone)
       (let ((phase (cavy-get-opt 'phase)))
         (or (eq phase nil)
             (eq phase 'compile)))
       (string= (cavy-get-opt 'target) "latex")))

(defun cavy-compile-command ()
  "Command to ready BUFFER used by PROCESS before receiving compilation output."
  (if  (cavy-pdf-preview-p)
      (cavy-build-pdf)
    (cavy-cli-command)))
      
(defun cavy-compile-buffer-setup (process buffer)
  "Command to ready BUFFER used by PROCESS before receiving compilation output."
  (cond ((cavy-pdf-preview-p)
         (cavy-compile-buffer-setup-latex process buffer))))

(defun cavy-compile-buffer-setup-latex (process buffer)
  "Prepare a BUFFER to view PDFs generated by PROCESS."
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

;;;;;;;;;;;;;;;;;
;; Indentation ;;
;;;;;;;;;;;;;;;;;

(defcustom cavy-indent-offset 4
  "TODO: indent Cavy code by this many spaces. This is currently unused!"
  :type 'integer
  :group 'cavy
  :safe #'integerp)

(defun cavy-indent-line ()
    "Indent a line of Cavy code."
  (indent-line-relative))

;;;;;;;;;;;;
;; Syntax ;;
;;;;;;;;;;;;

;; `define-derived-mode' will find this name automatically and use this table.
(defvar cavy-mode-syntax-table nil
  "Syntax table for `cavy-mode'.")

(setq cavy-mode-syntax-table
      (let ((syn-table (make-syntax-table)))
        (modify-syntax-entry ?\/ ". 12b" syn-table)
        (modify-syntax-entry ?\n "> b" syn-table)
        syn-table))

(defconst cavy-keywords
  '("if" "else" "for" "in" "match"
    "with" "as" "let" "fn" "impl"
    "mut" "struct" "enum" "type"
    "true" "false" "io" "assert"
    "drop")
  "Cavy keywords for font-locking.")

(defconst cavy-types
  '("bool" "u8" "u16" "u32" "Fn")
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
    (,(regexp-opt '("unsafe") 'symbols) . font-lock-warning-face)
    ("\\?" . 'cavy-special-operators-face)
    ("\\!" . 'cavy-special-operators-face))
    
  "Font-lock definitions.")

;;;;;;;;;;;;;;;;;;;;;
;; Mode definition ;;
;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-derived-mode cavy-mode prog-mode "Cavy"
  "Major mode for editing Cavylang code."

  ;; Fontification
  (setq-local font-lock-defaults '((cavy-font-lock-keywords)))

  ;; Indentation
  (setq-local indent-line-function #'cavy-indent-line)

  ;; Preview compiled code
  (add-hook 'after-save-hook 'cavy-after-save-hook nil t))

(provide 'cavy-mode)

;;; cavy-mode.el ends here
