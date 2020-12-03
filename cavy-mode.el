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

(defcustom cavy-compile-target 'qasm
  "What should be the output of the Cavy compiler?"
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

(defun cavy-compile-command ()
  "Determine the compilation command to run."
  (cond ((eq 'qasm cavy-compile-target)
         `(,cavy-binary "/dev/stdin" "-o" "/dev/stdout"))
        (t '("false"))))

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
)

(defun cavy-after-save-hook ()
  "Test function."
  (with-output-to-temp-buffer cavy-preview-buffer
    (cavy-compile-program cavy-preview-buffer)))

(defconst cavy-keywords
  '("if" "else" "for"
    "let" "fn" "print"
    "true" "false"
    )
  "Cavy keywords for font-locking.")

(defvar cavy-font-lock-keywords
  `(
    (,(regexp-opt cavy-keywords 'symbols) . font-lock-keyword-face)
    )
  "Font-lock definitions.")

(defvar cavy-syntactic-face-function
  (lambda (state)
    (if (nth 3 state) font-lock-string-face font-lock-comment-face)))

;;;###autoload
(define-derived-mode cavy-mode fundamental-mode "Cavy"
  "Major mode for editing Cavylang code."

  ;; Fontification
  (setq-local font-lock-defaults '((cavy-font-lock-keywords)))

  ;; Preview compiled code
  (add-hook 'after-save-hook 'cavy-after-save-hook nil t))

(provide 'cavy-mode)

;;; cavy-mode.el ends here
