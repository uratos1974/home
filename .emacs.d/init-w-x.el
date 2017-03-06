;;; -*- mode: emacs-lisp; coding: utf-8-unix; -*-
;;;
;;; Initialization file for Emacs on X
;;;
;;;


;; Mozc の設定
(use-package mozc
  :ensure t
  :config
  (setq default-input-method "japanese-mozc"))

;; フレームの設定
(setq default-frame-alist
      '((width . 120)
        (height . 40)
        (left . 0)
        (top . 0)
        (font . "Ricty-12")))


;;; End of File
