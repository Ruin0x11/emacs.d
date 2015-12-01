;; Let's see what we're running on
(setq on-console (null window-system))

;; Prevent the cursor from blinking
(blink-cursor-mode 0)
;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
;; Don't let Emacs hurt your ears
(setq visible-bell t)

;; You need to set `inhibit-startup-echo-area-message' from the
;; customization interface:
;; M-x customize-variable RET inhibit-startup-echo-area-message RET
;; then enter your username
(setq inhibit-startup-echo-area-message "ruin")

;; This is bound to f11 in Emacs 24.4
;; (toggle-frame-fullscreen) 
;; Who use the bar to scroll?
(scroll-bar-mode 0)

(tool-bar-mode 0)
(menu-bar-mode 0)

;; You can also set the initial frame parameters
;; (setq initial-frame-alist
;;       '((menu-bar-lines . 0)
;;         (tool-bar-lines . 0)))

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Show line numbers in buffers
(global-linum-mode t)
(setq linum-format (if on-console "%4d " "%4d"))

;; Show column numbers in modeline
(setq column-number-mode t)

;; Show current function in modeline
(which-function-mode)

;; Install themes
(package-require 'ample-theme)

(load-theme 'ample t)

;; Calculate default font size
(setq default-frame-font-size 10)
(setq presentation-frame-font-size
      (truncate (* 1.25 default-frame-font-size)))

;; Build font descriptor strings
;; (defun font-desc (name size)
;;   (concat "-unknown-" name "-normal-normal-normal-*-"
;;           (number-to-string size) "-*-*-*-m-0-iso10646-1"))
(defun font-desc (name size)
  (concat name " " (number-to-string size)))

;; Set default and presentation mode fonts
(defun default-frame-font ()
  (font-desc "Menlo for Powerline" default-frame-font-size))
(defun presentation-frame-font ()
  (font-desc "Menlo for Powerline" presentation-frame-font-size))
(set-frame-font (default-frame-font))

(provide 'ruin-theme)
