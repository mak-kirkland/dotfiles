;;;; PACKAGE.EL ;;;;

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;;;; USE-PACKAGE ;;;;

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;; SLY ;;;;

(use-package sly
  :load-path "~/git/sly"
  :hook (sly-mode . prettify-symbols-mode)
  :config
  (add-to-list 'sly-lisp-implementations '(sbcl ("sbcl"))))

;;;; CODE COMPLETION ;;;;

(use-package company
  :ensure t
  :init
  (global-company-mode)
  (setq company-require-match nil))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode))

(use-package yasnippet
  :ensure t
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

;;;; RIPGREP ;;;;

(use-package rg
  :ensure t
  :init
  (rg-enable-default-bindings))

;;;; GIT ;;;;

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;;;; RUST ;;;;

(setenv "PATH" (concat (getenv "PATH") ":" (getenv "HOME") "/.cargo/bin"))
(setq exec-path (append exec-path '("~/.cargo/bin")))

(use-package rustic
  :ensure t
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save t)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  ;; Customize symbol highlight face for stronger background highlight
  (set-face-attribute 'eglot-highlight-symbol-face nil
                      :background (face-background 'region)
                      :foreground (face-foreground 'default)
                      :weight 'bold))

;;;; APPEARANCE ;;;;

(use-package dired-sidebar
  :ensure t
  :bind (("C-x C-d" . dired-sidebar-toggle-sidebar))
  :config
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-theme 'nerd))

(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark t))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-frame-maximized)
(global-hl-line-mode nil)
;; Pretty lambda
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'lisp-mode-hook 'prettify-symbols-mode)

;;;; CUSTOM ;;;;

(setq default-directory "~/workspace/")
;; Disable startup screen
(setq inhibit-startup-screen t)
;; Emacs completion
(fido-vertical-mode 1)
;; Lisp mode in .cl files
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
;; Auto-revert buffers
(global-auto-revert-mode t)
;; Delimiter matching
(electric-pair-mode 1)
;; Typing over selection re-writes it
(delete-selection-mode)
;; Indentation and trailing whitespace
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Disable easy suspension (use M-x suspend-emacs)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-M-z") nil)
;; Save history across sessions
(savehist-mode 1)
;; Custom file
(setq custom-file "~/.emacs.d/emacs-custom-file-that-i-despise.el")
(load custom-file t)
