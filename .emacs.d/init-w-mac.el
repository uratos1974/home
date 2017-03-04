;;; -*- mode: emacs-lisp; coding: utf-8-unix; -*-
;;;
;;; Initialization file for Emacs on Mac
;;;
;;;


;; デスクトップ切替時にフォーカスが外れてしまう問題への対策
(menu-bar-mode 1)

;; Command/Option キーの設定
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

;; '¥' を '\' として扱う
(bind-key [?\¥] [?\\])
(bind-key [?\C-¥] [?\C-\\])
(bind-key [?\M-¥] [?\M-\\])
(bind-key [?\C-\M-¥] [?\C-\M-\\])

;; ミニバッファ入力時に日本語入力を OFF にする
(mac-auto-ascii-mode 1)

;; 日本語入力の状態に応じてカーソルの色を切り替える
(my/add-hook 'mac-selected-keyboard-input-source-change-hook
  (set-cursor-color
   (if (string-match "\\.Roman$" (mac-input-source))
       my/cursor-color-ime-off my/cursor-color-ime-on)))

;; フレームの設定
(setq default-frame-alist
      '((width . 100)
        (height . 46)
        (left . 0)
        (top . 0)
        (font . "Ricty-18")))


;;; End of File
