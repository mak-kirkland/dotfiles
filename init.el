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
  :hook  (emacs-startup . treemacs)
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window)
        ("C-x t t" . treemacs)
        ("C-x t B" . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file))
  :config
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
  :ensure t)

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
  :hook (svelte-mode . prettier-mode)
  :config
  (customize-set-variable 'svelte-basic-offset 4))

;;;; Eglot for LSP support ;;;;

(use-package eglot
  :ensure t
  :hook (svelte-mode . eglot-ensure)
  :hook (typescript-mode . eglot-ensure)
  :hook (json-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(json-mode . ("vscode-json-languageserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(typescript-mode .
                 (lambda (major-mode)
                   (if (derived-mode-p major-mode 'svelte-mode)
                       '("svelteserver" "--stdio")
                     '("typescript-language-server" "--stdio")))))
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

;;; Enable Prettier for CSS files
(use-package css-mode
  :hook (css-mode . prettier-mode))

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

;;;; ERGONOMICS ;;;;

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define-global "gg" 'keyboard-escape-quit))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-leader-define-key
   '("f" . find-file)
   '("p" . project-find-file)
   '("b" . switch-to-buffer)
   '("s" . save-buffer)
   '("x" . execute-extended-command)
   '("w" . other-window)
   '("v" . magit)
   '("r" . rg-project))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-comment)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("k" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)))

;; Now activate it:
(use-package meow
  :ensure t
  :config
  (meow-setup)
  (meow-global-mode 1))

(defun my/disable-meow-mode ()
  "Disable Meow minor mode in buffers that conflict with it."
  (meow-normal-mode -1)
  (meow-insert-mode -1)
  (meow-motion-mode -1)
  (meow-mode -1)) ;; Fully disables Meow keymaps

(dolist (hook '(magit-mode-hook
                magit-status-mode-hook
                magit-log-mode-hook
                magit-diff-mode-hook
                magit-refs-mode-hook
                magit-process-mode-hook
                magit-revision-mode-hook
                magit-blame-mode-hook))
  (add-hook hook #'my/disable-meow-mode))
