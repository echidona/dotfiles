;;package管理
;; straight.elで管理を行う
;; package管理のおまじない
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
(setq straight-use-package-by-default t)

;; color-theme
; (use-package zenburn-theme
;   :straight t)

;; path
;; シェルが読み込んでいるパスを読み込む
; (use-package exec-path-from-shell
;   :straight t
;   :config
;   (exec-path-from-shell-initialize))

;; helm
(use-package helm
  :straight t
  :bind (("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action))
  :config
  (helm-mode))

;; company
(use-package company
  :straight t
  :init
  (global-company-mode)
  (setq company-idle-delay 0) ;; 補完表示までの遅延は０秒
  (setq company-minimun-prefix-length 2) ;; 補完の行
  (setq company-selection-wrap-around t) ;; 一番下で次に行くと一番上の行に戻る
  :bind (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-s" . company-filter-candidates) ;; C-sで絞り込む
        ("C-n" . company-search-next)
        ("C-p" . company-search-previous))
  :config
  (defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

;; company jedi for python
; (use-package company-jedi
;   :straight t
;   :config
;   (defun my/python-mode-hook ()
;     (add-to-list 'company-backends 'company-jedi))
;   (add-hook 'python-mode-hook 'my/python-mode-hook))

;; undo-tree
(use-package undo-tree
  :straight t
  :bind ("M-/" . undo-tree-redo)
  :init
  (global-undo-tree-mode))

;; ;; linum
;; ;; show line number
;; (use-package linum
;;   :straight nil
;;   :config
;;   (line-number-mode t)
;;   (global-linum-mode t)
;;   (setq linum-delay t)
;;   (defadvice linum-schedule (around my-linum-schedule () activate)
;;     (run-with-idle-timer 0.2 nil #'linum-update-current)))

;; flycheck
(use-package flycheck
  :straight t
  :config
  (add-hook 'emacs-lisp-mode 'flycheck-mode)
  (add-hook 'cc-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'java-mode-hook 'flycheck-mode))

;; smartparens
(use-package smartparens
  :straight t
  :config
  (smartparens-global-mode))

;; yasnippet
(use-package yasnippet
  :straight t
  :init
  (setq yas-prompt-functions '(yas-ido-prompt))
  :bind
  (:map yas-keymap
  ("<tab>" . nil)) ;; companyと競合するのでyasnippetのフィールド移動は"C-i"のみにする
  :config
  (yas-global-mode))
(use-package yasnippet-snippets
  :straight t
  :config
  (setq yas-snippet-dirs
      '("~/.emacs.d/straight/repos/yasnippet-snippets/snippets")))

;; highlight-parentheses
(use-package highlight-parentheses
  :straight t
  :config
  (global-highlight-parentheses-mode t))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (use-package cl-lib
    :straight nil)
  (use-package color
    :straight nil)
  (defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))
  (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors))

;; redo+
(use-package redo+
  :straight t)

;; undohist
(use-package undohist
  :straight t
  :config
  (undohist-initialize)
  ;;; 永続化を無視するファイル名の正規表現
  (setq undohist-ignored-files
     '("/tmp/" "COMMIT_EDITMSG")))

;; recentf
; (use-package recentf
;   :straight t
;   :config
;   (recentf-mode t))

;; powerline
; (use-package powerline
;   :straight t
;   :config
;   (setq ns-use-srgb-colorspace nil)
;   (powerline-default-theme))

;; ;; htmlize (org-modeの追加package)
;; (use-package htmlize
;;   :straight t)

;; tramp (Emacsでリモート操作するためのpackage)
(use-package tramp
  :straight t
  :config
  (setq tramp-default-method "ssh"))

;; ace-isearch (バッファ内検索)
(use-package ace-isearch
  :straight t
  :init
  (use-package helm-swoop
    :straight t)
  (use-package avy
    :straight t)
  (use-package ace-jump-mode
    :straight t)
  :config
  (global-ace-isearch-mode)
  (custom-set-variables
 '(ace-isearch-function 'avy-goto-char)
 '(ace-isearch-use-jump 'printing-char)))


;; sequential-command (C-a C-aでファイルの先頭へ、C-e C-eでファイルの末尾へ)
; (use-package sequential-command
;   :straight t)

;; migemo
; (use-package migemo
;   :straight t
;   :init
;   (setq migemo-command "cmigemo")
;   (setq migemo-options '("-q" "--emacs"))
;   (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
;   (setq migemo-user-dictionary nil)
;   (setq migemo-regex-dictionary nil)
;   (setq migemo-coding-system 'utf-8-unix)
;   :config
;   (load-library "migemo")
;   (migemo-init))

;--------------------------------------
;;; tex mode
(add-hook 'latex-mode-hook
  '(lambda ()
     (setq tex-command "latexmk -pvc"))
     (setq tex-start-commands ""))

;; php-mode
; (use-package php-mode
;   :straight t)

;; Haskell-mode
; (use-package haskell-mode
;   :straight t)

;; python-mode
(use-package python-mode
  :straight t)

;;; setting markdown-mode
(use-package markdown-mode
  :straight t
  :config
  (setq markdown-enable-math t))
(use-package websocket
  :straight t)
(use-package web-server
  :straight t)
(use-package uuidgen
  :straight t)
(use-package markdown-preview-mode
  :straight t
  :config
  (autoload 'markdown-preview-mode "markdown-preview-mode.el" t)
  (setq markdown-preview-stylesheets (list "~/.emacs.d/other/github.css")))

;; setting agda-mode for automaton class
(use-package eaw
  :straight nil
  :load-path "site-lisp/"
  :config
  (eaw-fullwidth))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
;;-------------------------------------
;; tabサイズ
(setq default-tab-width 4)

;; インデントにタブを使用しない
(setq-default indent-tabs-mode nil)

;; 対応する括弧をハイライト
(show-paren-mode 1)

;; yes or no => y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;; 起動画面を表示しない
(setq inhibit-startup-screen t)

;; scratchメッセージを表示しない
(setq initial-scratch-message nil)

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

;; 時間を表示する
(display-time)

;; 選択領域を削除キーで一括削除
(delete-selection-mode t)

;; scroll in 1line
(setq scroll-step 1)
;---------------------------------------------------------------------------
;; キーの再設定
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>")) ;; mini bufferでもC-hでbackspaceを使う
(define-key global-map "¥" '[92]) ;; ¥マークを\に変える
(global-set-key (kbd "C-t") 'other-window)

(global-set-key (kbd "C-M-/") 'redo)
(global-set-key (kbd "C-c t") 'multi-shell-new)
(global-set-key (kbd "C-c p") 'multi-shell-prev)
(global-set-key (kbd "C-c n") 'multi-shell-next)

; setting character code
(set-language-environment "japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
