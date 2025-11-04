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

;;;; MODERN COMPLETION ;;;;

;; Vertico provides a minimal, high-performance vertical completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

;; Orderless provides flexible "out-of-order" filtering for Vertico/Corfu
(use-package orderless
  :ensure t
  :init
  ;; Set orderless as the default completion style
  (setq completion-styles '(orderless basic)
        ;; Keep categories nil to ensure orderless is used everywhere
        completion-category-defaults nil
        ;; EXCEPT for files, where we want traditional partial completion
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Marginalia adds helpful annotations (metadata) to minibuffer completions
(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode 1))

;; Corfu provides an in-buffer completion UI (replaces Company)
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  ;; Sane defaults to make Corfu feel like a modern, automatic popup
  (setq corfu-cycle t              ; Allow cycling through candidates
        corfu-auto t               ; Enable auto-completion
        corfu-preview-current nil  ; Don't preview current candidate
        corfu-separator ?\s        ; Use space as separator
        corfu-quit-at-boundary 'separator ; Hide popup when you type space
        corfu-quit-no-match 'separator    ; Hide popup if there are no matches
        corfu-auto-prefix 2        ; Start completion after 2 char
        corfu-auto-delay 0.1       ; Wait 0.1s before showing popup
        corfu-popupinfo-delay '(1.0 . 0.5)
        ))

;; Provides extra completion "brains" (backends) for Corfu
(use-package cape
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ; in-buffer
  (add-to-list 'completion-at-point-functions #'cape-file))   ; file paths

;; Consult provides powerful search/navigation commands using Vertico
(use-package consult
  :ensure t)

;; Icons for Corfu
(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Run M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

;; This package connects all-the-icons to Marginalia
(use-package all-the-icons-completion
  :ensure t
  :after marginalia
  :config
  (all-the-icons-completion-marginalia-setup))

;;;; ENVIRONMENT ;;;;

;; Tell Emacs where to find command-line tools
(setenv "PATH" (concat (getenv "PATH") ":" (getenv "HOME") "/.cargo/bin" ":" (getenv "HOME") "/.local/share/pnpm"))
(setq exec-path (append exec-path '("~/.cargo/bin" "~/.local/share/pnpm")))

;;;; LANGUAGES & TOOLS ;;;;

;;;; GIT ;;;;
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;;;; FORMATTING ;;;;
(use-package prettier
  :ensure t)

;;;; SLY ;;;;
(use-package sly
  :load-path "~/git/sly"
  :hook (sly-mode . prettify-symbols-mode)
  :config
  (add-to-list 'sly-lisp-implementations '(sbcl ("sbcl"))))

;;;; RUST ;;;;
(use-package rustic
  :ensure t
  :config
  (setq rustic-lsp-client nil)
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

;;;; TYPESCRIPT ;;;;
(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode))
  :hook (typescript-mode . prettier-mode))

;;;; JSON ;;;;
(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.jsonc\\'" . json-mode) ; For JSON with comments
         ("\\.prettierrc\\'" . json-mode))
  :hook (json-mode . prettier-mode)) ; Optional: Auto-format on save

;;;; YAML ;;;;
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :hook (yaml-mode . prettier-mode)) ; Optional: Auto-format on save

;;;; CSS ;;;;
(use-package css-mode
  :hook (css-mode . prettier-mode))

;;;; Eglot for LSP support ;;;;
(use-package eglot
  :ensure t
  :hook ((svelte-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (json-mode . eglot-ensure)
         (rustic-mode . eglot-ensure))
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

;;;; CUSTOM ;;;;

(setq default-directory "~/workspace/")
;; Disable startup screen
(setq inhibit-startup-screen t)
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
   '("b" . consult-buffer)
   '("s" . save-buffer)
   '("x" . execute-extended-command)
   '("w" . other-window)
   '("v" . magit)
   '("r" . consult-ripgrep))
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
