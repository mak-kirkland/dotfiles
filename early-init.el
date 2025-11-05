;;;; EARLY INIT ;;;;
;; This file loads before packages and themes

;; Disable UI elements before they are ever drawn
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Maximize the frame on creation
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Inhibit the startup screen
(setq inhibit-startup-screen t)
