;;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Allegro Manual
(load "~/the-vaults/emacs-libs/fi-manual.el")

;;;; SLY ;;;;

(add-to-list 'load-path "~/git/sly")
(require 'sly-autoloads)

(with-eval-after-load 'sly
  (add-to-list 'sly-lisp-implementations
               '(sbcl ("sbcl")))
  (add-to-list 'sly-lisp-implementations
               '(allegro ("~/opt/acl10.1-smp.64/alisp")))
  (add-function
   :around sly-find-buffer-package-function
   (lambda (oldfun &rest args)
     (let ((val (apply oldfun args)) )
       (and val
            (replace-regexp-in-string "^.com\\.ravenpack\\." "" val))))
   '((name . joaot/remove-very-long-package-prefix))))

;;;; PACKAGES ;;;;

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-require-match nil))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode))

(use-package rg
  :init
  (rg-enable-default-bindings))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;;;; CUSTOM ;;;;

;; Upcase/downcase region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; Lisp mode in .cl files
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
;; Auto-revert buffers
(global-auto-revert-mode t)
;; Default browser is Emacs built-in browser
(setq browse-url-browser-function 'eww-browse-url)
;; Delimiter matching
(electric-pair-mode 1)
;; Find file in git project
(global-set-key (kbd "C-c p") 'project-find-file)
;; Completion
(fido-mode 1)
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

;;;; APPEARANCE ;;;;

(setq inhibit-startup-screen t)
(show-paren-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-frame-maximized)

(setq custom-file "~/tmp/emacs-custom-file-that-i-despise.el")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'atom-one-dark t)
(set-frame-font "Terminus-16" nil t)

;; Pretty lambda
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'sly-mode-hook 'prettify-symbols-mode)
