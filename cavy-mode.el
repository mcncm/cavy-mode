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

(defun cavy-after-save-hook ()
  "Test function."
  (with-output-to-temp-buffer "cavy-preview"
    (print "You're writing Cavy, aren't you?")))

;;;###autoload
(define-derived-mode cavy-mode fundamental-mode "Cavy"
  "Major mode for editing Cavylang code."

  (add-hook 'after-save-hook 'cavy-after-save-hook nil t))

(provide 'cavy-mode)

;;; cavy-mode.el ends here
