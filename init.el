; stright.el bootstrap-code
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; use-packageをインストールする
(straight-use-package 'use-package)
;; オプションなしで自動的にuse-packageをstraight.elにフォールバックする
;; 本来は (use-package hoge :straight t) のように書く必要がある
(setq straight-use-package-by-default t)

;; theme
(use-package atom-one-dark-theme
  :config (set-face-background 'default "unspecified-bg"))
;; mode-lineを書き換え
(use-package smart-mode-line
  :init
  (defvar sml/no-confirm-load-theme t)
  :config
  (sml/setup))

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

;; company(文字列補完)
(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  (set-face-attribute 'company-tooltip nil
                    :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
		      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
		      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
		      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
		      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
		      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
		      :background "gray40")
  :bind
  ("C-M-i" . company-complete)
  (:map company-active-map
	("C-n" . 'company-select-next)
	("C-p" . 'company-select-previous)
	("C-s" . 'company-filter-candidates)
	([tab] . 'company-complete-selection))
  (:map company-search-map
	("C-n" . 'company-select-next)
	("C-p" . 'company-select-previous)))

;source code系-----------------------
;;tex
;;; tex mode
(add-hook 'latex-mode-hook
  '(lambda ()
     (setq tex-command "latexmk -pvc"))
     (setq tex-start-commands ""))

;pakages---------------------------------------------------
;; 余計な物を表示しない
; (tool-bar-mode -1) ; GUI
; (scroll-bar-mode -1) ; GUI
(menu-bar-mode -1)

(global-hl-line-mode 0)
;; auto insert closing bracket
(electric-pair-mode 1)
;; turn on bracket match highlight
(show-paren-mode 1)
;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; show cursor position within line
(column-number-mode 1)
;; stop creating those backup~ files
(setq make-backup-files nil)
;; stop creating those #auto-save# files
(setq auto-save-default nil)

;keybind-----------------------------------
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>")) ;; mini bufferでもC-hでbackspaceを使う
