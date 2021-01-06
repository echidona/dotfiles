;; enable syntax hilight
(global-font-lock-mode 1)
;; autoinsert pair brackets
(electric-pair-mode 1)
(show-paren-mode t)
;; use space for autoindent
(setq-default indent-tabs-mode nil)
;; overwrite on selection region
(delete-selection-mode t)
;; show line and column number
(line-number-mode t)
(column-number-mode t)
;; don't show menu bar and tool bar
(menu-bar-mode -1)
; (tool-bar-mode -1)
(setq inhibit-startup-message t)
;; copy to clipboard
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)

;; don't make backup and save file
(setq auto-save-default nil)
(setq make-backup-files nil)

;; keybind
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key (kbd "C-x C-b") 'bs-show)
;; bs (show scratch buffer in baffer show)
(setq bs-default-configuration "files-and-scratch")

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
(straight-use-package 'use-package)

(use-package ivy
  :straight t
  :bind (("\C-s" . swiper-isearch-region))
  :config
  (ivy-mode 1)
  (use-package swiper
    :straight t
    :config
    (defun swiper-isearch-region ()
      "If region isn't selected, `swiper-isearch'.
       If region is selected, `swiper-isearch-thing-at-point'."
      (interactive)
      (if (not (use-region-p))
          (swiper-isearch))
      (swiper-isearch-thing-at-point))))

(use-package company
  :straight t
  :init
  (global-company-mode t)
  :config
  (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
  (setq company-idle-delay 0) ; デフォルトは0.5
  (setq company-minimum-prefix-length 3) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  :bind (("C-M-i" . company-complete)
	 :map company-active-map
	 ("C-n" . company-select-next)
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next)
	 ("C-p" . company-select-previous)
	 ("C-s" . company-filter-candidates)
	 ("C-i" . company-complete-selection)
         ("C-h" . 'delete-backward-char)
	 ([tab] . company-complete-selection)
	 ("C-f" . company-complete-selection)))

(use-package dockerfile-mode
  :straight t
  :mode ("Dockerfile\\'" . dockerfile-mode))
(use-package docker-compose-mode
  :straight t)

(use-package whitespace
  :straight t
  :config
  (setq whitespace-style '(face
                           trailing))
  (set-face-attribute 'whitespace-trailing nil
                      :foreground "RoyalBlue4"
                      :background "RoyalBlue4"
                      :underline nil)
  (global-whitespace-mode 1))

(use-package markdown-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  (setq markdown-fontify-code-blocks-natively t))

(use-package fish-mode
  :straight t)

(use-package lsp-mode
  :straight t
  :config
  (use-package lsp-latex
    :straight t)
  (use-package tex-mode
    :straight t
    :init
    (add-hook 'tex-mode-hook 'lsp)
    (add-hook 'latex-mode-hook 'lsp))
  (use-package bibtex
    :straight t
    :init
    (add-hook 'bibtex-mode-hook 'lsp))
  (setq lsp-latex-build-executable "lualatex")
  (setq lsp-latex-build-args '("%f"))
  (setq lsp-latex-build-on-save t))

(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'python-mode-hook 'rainbow-delimiters-mode))

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  :bind
  ("C-z" . 'undo-tree-undo)
  ("C-M-z" . 'undo-tree-redo))

(use-package browse-kill-ring
  :straight t
  :bind (("M-y" . 'browse-kill-ring)))
