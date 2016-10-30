;;; -*- mode: emacs-lisp; coding: utf-8-unix; -*-
;;;
;;; Initialization file for Emacs on Windows
;;;
;;;


;; デフォルト文字コード
(prefer-coding-system 'utf-8-dos)

;; Windows/Apps キーを Super/Hyper キーとして扱う
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)
(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super)
(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper)

;; 日本語入力
(setq default-input-method "W32-IME")
(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
(setq-default w32-ime-mode-line-state-indicator "[--]")
(w32-ime-initialize)
(bind-key "<convert>"     'ignore)
(bind-key "<non-convert>" 'ignore)

;; IME の状態に応じてカーソルの色を切り替える
(my/add-hook 'w32-ime-off-hook
  (set-face-attribute 'cursor nil :background my/cursor-color-ime-off))
(my/add-hook 'w32-ime-on-hook
  (set-face-attribute 'cursor nil :background my/cursor-color-ime-on))

;; migemo
(setq migemo-command "C:/Applications/CMigemo/cmigemo.exe")
(setq migemo-dictionary "C:/Applications/CMigemo/dict/utf-8/migemo-dict")

;; migemo 使用時に IME を自動 OFF する
(setq migemo-mw32-input-method default-input-method)

;; helm 使用時に IME が勝手に切り替わるのを防ぐ
(my/advice-add 'helm :around
  (let ((select-window-functions nil))
    (apply func args)))

;; 見た目
(setq default-frame-alist
      '((top . 0)
        (left . -1)
        (width . 100)
        (height . 62)
        (font . "MeiryoKe_Console-12")
        (line-spacing . 2)))


;;; End of File
