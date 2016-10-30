;;; -*- mode: emacs-lisp; coding: utf-8-unix; -*-
;;;
;;; Initialization file for Emacs on X
;;;
;;;


(require 'anthy)
(global-set-key (kbd "s-SPC") 'toggle-input-method)
(define-key anthy-preedit-keymap (kbd "<escape>") (kbd "C-g"))
(setq anthy-preedit-begin-mark "")
(setq anthy-preedit-delim-mark "")
(setq anthy-accept-timeout 1)
(setq default-frame-alist '((top . 0) (left . -1) (width . 120) (height . 40) (font . "Ricty-14")))


;;; End of File
