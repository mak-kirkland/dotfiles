;;;; PACKAGE.EL ;;;;

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;;;; USE-PACKAGE ;;;;

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;; APPEARANCE ;;;;

(use-package treemacs
  :ensure t
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window)
        ("C-x t t" . treemacs)
        ("C-x t B" . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file))
  :config
    ;; Single click to open files
  (setq treemacs-mouse-1-single-click-expand t)
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)))

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

;;;; FORMATTING ;;;;

(use-package prettier
  :ensure t
  :hook (svelte-mode . prettier-mode))

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
  (setq company-require-match nil)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

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

;;;; SVELTE ;;;;

(use-package svelte-mode
  :ensure t
  :mode ("\\.svelte\\'" . svelte-mode)
  :hook (svelte-mode . prettier-mode))

;;;; Eglot for LSP support ;;;;

(use-package eglot
  :ensure t
  :hook (svelte-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))
  ;; Customize symbol highlight face for stronger background highlight
  (set-face-attribute 'eglot-highlight-symbol-face nil
                      :background (face-background 'region)
                      :weight 'bold)
  (set-face-attribute 'eglot-diagnostic-tag-unnecessary-face nil
                      :inherit 'flymake-warning))

;;; Configuration for JSON files
(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.jsonc\\'" . json-mode) ; For JSON with comments
         ("\\.prettierrc\\'" . json-mode))
  :hook (json-mode . prettier-mode)) ; Optional: Auto-format on save

;;; Configuration for YAML files
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :hook (yaml-mode . prettier-mode)) ; Optional: Auto-format on save

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
