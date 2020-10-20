(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  (require 'use-package))

;;;; RAVENPACK ;;;;

(setenv "ACL_LOCALE" "C.latin1")
(setenv "ORACLE_HOME"     (concat (getenv "HOME") "/opt/oracle/"))
(setenv "LD_LIBRARY_PATH" (concat (getenv "HOME") "/opt/oracle/"))
(setenv "TNS_ADMIN"       (concat (getenv "HOME") "/workspace/configuration/"))

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

(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

;;;; PACKAGES ;;;;

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (company-quickhelp-mode)
  (setq company-require-match nil))

(use-package rg
  :init
  (rg-enable-default-bindings)
  (setf rg-group-result t)
  (setf rg-buffer-name "rg"))

(use-package magit
  :bind ("C-x g" . magit-status))

;;;; CUSTOM ;;;;

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
(set-frame-font "-xos4-terminus-medium-r-normal--16-*-72-72-c-80-*-r")

(defun use-terminal-background ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook #'use-terminal-background)

;; Pretty lambda
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'sly-mode-hook 'prettify-symbols-mode)
