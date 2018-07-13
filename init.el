; package管理
; straight.elで管理を行う
; package管理のおまじない
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; packageのインストール
;; packageはuse-packageで管理
;; (straight-use-package 'package-pac)でインストールされていなければ、新たにインストール
;; (require 'package-pac)で有効化
;; packageの有効化
(straight-use-package 'use-package)
(require 'use-package)
;; helm
(use-package helm
  :straight t)
(require 'helm) ;; hemlの有効化
;; company
(use-package company
  :straight t)
(require 'company) ;; companyの有効化(補完機能)
(global-company-mode)
(setq company-idle-delay 0) ;; 補完表示までの遅延は０秒
(setq company-minimun-prefix-length 2) ;; 補完の行
(setq company-selection-wrap-around t) ;; 一番下で次に行くと一番上の行に戻る
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
	    '(with company-yasnippet))))
(setq company-backend (mapcar #'company-mode/backend-with-yas company-backends))
;; undo-tree
(use-package undo-tree
  :straight t)
(require 'undo-tree) ;; undo-treeの有効化
(global-undo-tree-mode t)
;; color-theme
(use-package madhat2r-theme
  :straight t)
(require 'madhat2r-theme)
;; linum
(use-package linum
  :straight t)
(require 'linum) ;; linumの有効化
(global-linum-mode t)
;; flycheck
(use-package flycheck
  :straight t)
(require 'flycheck) ;; flycheckの有効化
(add-hook 'cc-mode-hook 'flycheck-mode)
;; smartparens
(use-package smartparens
  :straight t)
(require 'smartparens-config) ;; smartparensの有効化
(smartparens-global-mode t)
;; yasnippet
(use-package yasnippet
  :straight t)
(use-package yasnippet-snippets
  :straight t)
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
	"~/.emacs.d/straight/repos/yasnippet-snippets/snippets"))
(eval-after-load "yasnippet"
  '(progn
     ;; companyと競合するのでyasnippetのフィールド移動は"C-i"のみにする
     (define-key yas-keymap (kbd "<tab>") nil)
     (yas-global-mode 1)))
(setq yas-prompt-functions '(yas-ido-prompt))
;; highlight-parentheses
(use-package highlight-parentheses
  :straight t)
(require 'highlight-parentheses)
(global-highlight-parentheses-mode t)
;; rainbow-delimiters
(use-package rainbow-delimiters
  :straight t)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; 括弧の色を強調する設定
(require 'cl-lib)
(require 'color)
(defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))
(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)
;; redo+
(use-package redo+
  :straight t)
(require 'redo+)
;; undohist
(use-package undohist
  :straight t)
(require 'undohist)
(undohist-initialize)
;;; 永続化を無視するファイル名の正規表現
(setq undohist-ignored-files
      '("/tmp/" "COMMIT_EDITMSG"))
;; php-mode
(use-package php-mode
  :straight t)
(require 'php-mode)
;;----------------------------------------------------------------------------
;; ツールバーやメニューバー、スクロールバーを非表示
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
;; tabサイズ
(setq default-tab-width 4)
;; 対応する括弧をハイライト
(show-paren-mode 1)
;; yes or no => y or n
(defalias 'yes-or-no-p 'y-or-n-p)
;; タイトルにフルパスを表示
(setq frame-title-format "%f")
;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)
;; 大文字・小文字を区別しない
(setq case-fold-search t)
;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)
;; バックアップファイルの作成先を~/.emacs.d/backupに変更
(setq backup-directory-alist
      (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
	    backup-directory-alist))
;; バックアップファイルを複数バージョンで保存
(setq version-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 1)
(setq delete-old-versions t)
;; 列数を表示する
(column-number-mode t)
;; カーソル行をハイライトする
(global-hl-line-mode t)
;; beep音を消す
(defun my-bell-function ()
  (unless (memq this-command
		'(isearch-abort abort-recursive-edit exit-minibuffer
				keyboard-quit mwheel-scroll down up next-line previous-line
				backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)
;; キーの再設定
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>")) ;; mini bufferでもC-hでbackspaceを使う
(define-key global-map "¥" '[92]) ;; ¥マークを\に変える
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "M-/") 'undo-tree-redo)
(global-set-key (kbd "C-M-/") 'redo)
