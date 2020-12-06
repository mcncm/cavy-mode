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

(defvar cavy-qasm-compile-command
  `(,cavy-binary "/dev/stdin" "-o" "/dev/stdout")
  "Command to run when compiling to qasm.")

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
                  " /dev/stdin -o " tex-doc " --target latex; "
                  ;; And compile it! Note that latexmk is very noisy, and we
                  ;; have to supress all its output in order for us to get a
                  ;; well-formed pdf out of this.
                  "latexmk " tex-doc " -pdf > /dev/null 2>&1; "
                  "cat " pdf-doc)))
  `("bash" "-c" ,shell-script)))

(defun cavy-pycavy-latex-compile-command ()
  "Command to run when compiling to latex, using pycavy as an intermediary."
  (let* ((python-program
          (string-join
           '("import sys"
           "from pycavy import Program"
           "src = sys.stdin.read()"
           "Program(src).compile().to_diagram().to_pdf(\'$DIR/cavy\')")
           "; "))
        (shell-script
         (concat "DIR=`mktemp -d`; "
                 "python -c " (concat "\"" python-program "\"") "; "
                 "cat $DIR/cavy.pdf")
          ))
    `("bash" "-c" ,shell-script)))

(defun cavy-compile-command ()
  "Determine the compilation command to run."
  (cond ((eq 'qasm cavy-compile-target)
         cavy-qasm-compile-command)
        ((eq 'latex cavy-compile-target)
         (cond ((eq 'pycavy cavy-latex-compile-method)
                (cavy-pycavy-latex-compile-command))
               ((eq 'native cavy-latex-compile-method)
                (cavy-native-latex-compile-command))
               (t '("false"))))
        (t '("false"))))

(defun cavy-compile-buffer-setup (buffer)
  "Command to ready BUFFER before receiving compilation output."
  (cond ((eq 'latex cavy-compile-target)
          (cavy-compile-buffer-setup-latex buffer))))

(defun cavy-compile-buffer-setup-latex (buffer)
  "Prepare a BUFFER to view PDFs."
  (with-current-buffer (get-buffer buffer)
    (progn
      ;; Default encoding is UTF-8, which doesn't accept some bytes in the PDF
      ;; headers.
      (set-buffer-file-coding-system 'raw-text)
      (pdf-view-mode))))

(defun cavy-compile-process (buffer)
  "Make a Cavy compilation process in buffer BUFFER."
  (make-process :name "cavy" :buffer buffer :command (cavy-compile-command)))

(defun cavy-compile-program (buffer)
  "Compile a program and return the object code.
The BUFFER argument is the buffer to write output to."
  (let ((process (cavy-compile-process buffer)))
    (process-send-string process (buffer-string))
    (process-send-eof process)
    (accept-process-output process))
  (cavy-compile-buffer-setup buffer))

(defun cavy-after-save-hook ()
  "Test function."
  (with-output-to-temp-buffer cavy-preview-buffer
    (cavy-compile-program cavy-preview-buffer)))

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
  '("if" "else" "for"
    "let" "fn" "print"
    "true" "false")
  "Cavy keywords for font-locking.")

(defvar cavy-font-lock-keywords
  `(
    (,(regexp-opt cavy-keywords 'symbols) . font-lock-keyword-face)
    )
  "Font-lock definitions.")

; (defvar cavy-syntactic-face-function
;   (lambda (state)
;     (if (nth 3 state) font-lock-string-face font-lock-comment-face)))

;;;;;;;;;;;;;;;;;;;;;
;; Mode definition ;;
;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-derived-mode cavy-mode fundamental-mode "Cavy"
  "Major mode for editing Cavylang code."

  ;; Fontification
  (setq-local font-lock-defaults '((cavy-font-lock-keywords)))

  ;; Preview compiled code
  (add-hook 'after-save-hook 'cavy-after-save-hook nil t))

(provide 'cavy-mode)

;;; cavy-mode.el ends here
