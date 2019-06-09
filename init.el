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
;; mode-lineの設定
;;; in active
(set-face-attribute 'mode-line nil :foreground "#007090" :background "#000000")
;;; no active
(set-face-attribute 'mode-line-inactive nil :foreground "#202020" :background "#000000")

;; helm(コマンド補完)
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
(use-package helm-swoop
  :bind ("C-s" . helm-swoop))

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
  ;; yasnippetとの連携
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
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

; カッコを綺麗に表示する
(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode))

;; project treeを表示
(use-package neotree
  :bind ([f8] . 'neotree-toggle)
  :config
  (use-package all-the-icons)
  (setq neo-show-hidden-files t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (custom-set-faces
   '(neo-root-dir-face ((t (:foreground "#8D8D84"))))
   '(neo-dir-link-face ((t (:foreground "#0000FF"))))
   '(neo-file-link-face ((t (:foreground "#BA36A5"))))))

;;source code系-----------------------
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (use-package yasnippet-snippets)
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"))
  (yas-global-mode 1))

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
;; mini bufferでもC-hでbackspaceを使う
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
;; C-oで別のwindowへ移る
(global-set-key (kbd "C-o") 'other-window)
