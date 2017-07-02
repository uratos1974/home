;;; -*- mode: emacs-lisp; coding: utf-8-unix; -*-
;;;
;;; Initialization file for Emacs
;;;
;;;


;;; 便利関数＆マクロ ---------------------------------------------------

;; リストの末尾に要素を追加する。要素は複数個指定可能。リスト内に既存の要
;; 素は追加しない
(defmacro my/append-to-list (list-var &rest elements)
  (declare (indent 1))
  `(dolist (element (list ,@elements))
     (add-to-list ,list-var element t)))

;; フックを追加する。lambda 無しで式を直接列挙できる
(defmacro my/add-hook (hook &rest body)
  (declare (indent defun))
  `(add-hook ,hook (lambda () ,@body)))

;; アドバイスを追加する。lambda 無しで式を直接列挙できる
(defmacro my/advice-add (symbol where &rest body)
  (declare (indent defun))
  `(advice-add ,symbol ,where (lambda (func &rest args) ,@body)))


;;; パッケージ管理 -----------------------------------------------------

;; パッケージ管理機能の初期化
(require 'package)
(my/append-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; use-package を有効化
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;;; face ---------------------------------------------------------------

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

;; IME の状態に応じたカーソルの色 (色の切り替え処理は環境毎に実装する)
(defvar my/cursor-color-ime-off (face-attribute 'cursor :background))
(defvar my/cursor-color-ime-on  (face-attribute 'font-lock-keyword-face :foreground))


;;; キーバインディング -------------------------------------------------

(keyboard-translate ?\C-h ?\C-?)
(bind-key "C-j" 'reindent-then-newline-and-indent)

;; よく使う機能を集約するためのキーマップ
(define-prefix-command 'my/menu-root-map)
(bind-key "C-;" 'my/menu-root-map)

;; ジャンプ系の機能を集約するためのキーマップ
(define-prefix-command 'my/menu-jump-map)
(bind-key "j" 'my/menu-jump-map my/menu-root-map)
(bind-key "l" 'goto-line my/menu-jump-map)


;;; 雑多な設定 ---------------------------------------------------------

;; auto-fill-mode と折り返しの設定
(auto-fill-mode -1)
(setq-default fill-column 72)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; その他の minor mode
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 1)
(fringe-mode nil)
;; (global-linum-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)
(delete-selection-mode 1)

;; 日本語
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)

;; 改行コードの表記
(setq eol-mnemonic-dos       ":D:")
(setq eol-mnemonic-mac       ":M:")
(setq eol-mnemonic-unix      ":U:")
(setq eol-mnemonic-undecided ":?:")

;; カーソル移動によるスクロールを１行単位に
(setq scroll-conservatively 1)
(setq scroll-margin 1)
(setq next-screen-context-lines 1)

;; マウスホイールによるスクロールを３行単位に
(setq mouse-wheel-scroll-amount '(3))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)

;; マウスホイールでフォントサイズを変更可能に
(bind-key "<C-wheel-up>"   (lambda () (interactive) (text-scale-increase 1)))
(bind-key "<C-wheel-down>" (lambda () (interactive) (text-scale-decrease 1)))

;; タブ
(setq-default tab-width 8)
(setq-default indent-tabs-mode t)

;; いろいろ
(setq warning-minimum-level :error)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq visible-bell t)
(setq linum-format "%5d ")
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq vc-follow-symlinks t)
(setq ad-redefinition-action 'accept)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(my/add-hook 'after-save-hook (executable-make-buffer-file-executable-if-script-p))


;;; auto-complete - 自動補完 -------------------------------------------

(use-package auto-complete-config
  :ensure auto-complete
  :config
  (setq ac-use-menu-map t)
  (ac-config-default))


;;; helm - 補完/選択インタフェースを強化 -------------------------------

(use-package helm-config
  :ensure helm
  :bind (("C-x C-f" . helm-find-files)
         ("C-x b"   . helm-buffers-list)
         ("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         ("<f1> a"  . helm-apropos)))

(use-package helm-files
  :defer 3
  :bind (:map helm-find-files-map
              ("<tab>"   . helm-execute-persistent-action)
              ("<S-tab>" . helm-find-files-up-one-level)))

(use-package helm-buffers :defer 3)
(use-package helm-command :defer 3)
(use-package helm-ring    :defer 3)
(use-package helm-elisp   :defer 3)


;;; helm-gtags - helm で GNU GLOBAL を使う -----------------------------

(use-package helm-gtags
  :ensure t
  :bind (:map my/menu-jump-map
              ("f" . helm-gtags-find-pattern)
              ("j" . helm-gtags-dwim)
              ("p" . helm-gtags-pop-stack)))


;;; Magit - Git インタフェース -----------------------------------------

(use-package magit
  :ensure t
  :bind (:map my/menu-root-map ("g" . magit-status)))


;;; migemo - ローマ字で日本語検索 --------------------------------------

(use-package migemo
  :ensure t
  :config
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix))


;;; multiple-cursors - 複数箇所を同時に編集 ----------------------------

(use-package multiple-cursors
  :ensure t
  :bind (("M-SPC" . set-rectangular-region-anchor)))


;;; popwin - 特定のバッファをポップアップ表示にする --------------------

(use-package popwin
  :ensure t
  :config
  (my/append-to-list 'popwin:special-display-config "*Backtrace*")
  (popwin-mode 1))


;;; undo-tree - undo を強化 --------------------------------------------

(use-package undo-tree
  :ensure t
  :bind (("M-/" . undo-tree-redo))
  :config
  (global-undo-tree-mode))


;;; ファイル保存時に行末の空白文字を取り除く ---------------------------

;; この変数に t をセットすると機能を無効化できる (バッファローカル)
(defvar my/inhibit-delete-trailing-whitespace nil)

(make-variable-buffer-local 'my/inhibit-delete-trailing-whitespace)
(defun my/delete-trailing-whitespace ()
  (interactive)
  (unless my/inhibit-delete-trailing-whitespace
    (delete-trailing-whitespace)))
(my/add-hook 'before-save-hook (my/delete-trailing-whitespace))


;;; text-mode ----------------------------------------------------------

(my/add-hook 'text-mode-hook
  (auto-fill-mode 1))


;;; markdown-mode ------------------------------------------------------

(use-package markdown-mode
  :ensure t
  :config
  (my/add-hook 'markdown-mode-hook
    (setq my/inhibit-delete-trailing-whitespace t)))


;;; prog-mode ----------------------------------------------------------

(my/add-hook 'prog-mode-hook
  (setq tab-width 4)
  (setq indent-tabs-mode nil))


;;; c-mode -------------------------------------------------------------

(my/add-hook 'c-mode-hook
  (c-set-style "linux")
  (setq c-basic-offset 4)
  (c-toggle-auto-hungry-state 1)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent nil)
  (helm-gtags-mode 1))


;;; 他の設定ファイルを読み込む -----------------------------------------

;; ~/.emacs.d/init-*.el を読み込むためのマクロ
(defmacro my/load-init-file (&rest args)
  `(load (locate-user-emacs-file (concat "init-" ,@args)) t))

;; ウィンドウシステム毎の設定ファイル
(my/load-init-file "w-" (symbol-name window-system))

;; ホスト毎の設定ファイル
(my/load-init-file "h-" (replace-regexp-in-string "\\..*" "" (system-name)))


;;; Emacs をサーバ化する -----------------------------------------------

(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t)
(setq server-socket-dir user-emacs-directory)
(unless (server-running-p) (server-start))


;;; End of File
