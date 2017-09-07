;;; ruin-theme.el --- theming and appearance setup

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

;; Set frame title
(setq frame-title-format '(multiple-frames "%b" ("" invocation-name "@" system-name ": Grow a world." )))

;; You can also set the initial frame parameters
;; (setq initial-frame-alist
;;       '((menu-bar-lines . 0)
;;         (tool-bar-lines . 0)))

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Show line numbers in buffers

;; global linum mode causes freeze on executing org-agenda custom commands
;; performance with org-mode is generally terrible
;; http://emacs.stackexchange.com/a/18419
;; http://stackoverflow.com/q/5229705
(require 'linum-off)
(global-linum-mode t)

(setq linum-format (if on-console "%4d " "%4d"))

;; Show column numbers in modeline
(setq column-number-mode t)

;; Show current function in modeline
(which-function-mode)

;; Show whitespace
(require 'whitespace)
(setq whitespace-style '(face trailing))
(global-whitespace-mode 1)

;; Install themes
;; (package-require 'ample-theme)
;; (package-require 'spacemacs-theme)
;; (package-require 'zenburn-theme)
(package-require 'solarized-theme)
(package-require 'monokai-theme)
(package-require 'spaceline)
(require 'spaceline-config)

(setq custom-theme-directory (locate-user-emacs-file "themes"))
(setq custom-theme-load-path (add-to-list 'custom-theme-load-path
                                          (locate-user-emacs-file "themes/btcsb")))

;;; Properly align CJK org-mode tables
;; https://github.com/kuanyui/.emacs.d/blob/master/rc/rc-basic.el#L102
(defun setup-cjk-alignment ()
  (when (display-graphic-p)
    (defvar emacs-english-font "Iosevka Light"
      "The font name of English.")
 
    (defvar emacs-cjk-font "東風ゴシック" "The font name for CJK.")

    (defvar emacs-font-size-pair '(14 . 14)
      "Default font size pair for (english . chinese)")

    (defvar emacs-font-size-pair-list
      '(( 5 .  6) (9 . 10) (10 . 12)(12 . 14)
        (13 . 16) (15 . 18) (17 . 20) (19 . 22)
        (20 . 24) (21 . 26) (24 . 28) (26 . 32)
        (28 . 34) (30 . 36) (34 . 40) (36 . 44))
      "This list is used to store matching (englis . chinese) font-size.")

    (defun font-exist-p (fontname)
      "Test if this font is exist or not."
      (if (or (not fontname) (string= fontname ""))
          nil
        (if (not (x-list-fonts fontname)) nil t)))

    (defun set-font (english chinese size-pair)
      "Setup emacs English and Chinese font on x window-system."

      (if (font-exist-p english)
          (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))

      (if (font-exist-p chinese)
          (dolist (charset '(kana han symbol cjk-misc bopomofo))
            (set-fontset-font (frame-parameter nil 'font) charset
                              (font-spec :family chinese :size (cdr size-pair))))))
    ;; Setup font size based on emacs-font-size-pair
    (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)

    (defun emacs-step-font-size (step)
      "Increase/Decrease emacs's font size."
      (let ((scale-steps emacs-font-size-pair-list))
        (if (< step 0) (setq scale-steps (reverse scale-steps)))
        (setq emacs-font-size-pair
              (or (cadr (member emacs-font-size-pair scale-steps))
                  emacs-font-size-pair))
        (when emacs-font-size-pair
          (message "emacs font size set to %.1f" (car emacs-font-size-pair))
          (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))

    (defun increase-emacs-font-size ()
      "Decrease emacs's font-size acording emacs-font-size-pair-list."
      (interactive) (emacs-step-font-size 1))

    (defun decrease-emacs-font-size ()
      "Increase emacs's font-size acording emacs-font-size-pair-list."
      (interactive) (emacs-step-font-size -1))

    ;; (global-set-key (kbd "C-=") 'increase-emacs-font-size)
    ;; (global-set-key (kbd "C--") 'decrease-emacs-font-size)
    ))

(setq spaceline-workspace-numbers-unicode 't)

(defface ruin/warning-face '((t (:foreground "white" :background "cadetblue")))
  "Face for a warning.")

(spaceline-define-segment org-clock
  "Show information about the current org clock task.  Configure
`spaceline-org-clock-format-function' to configure. Requires a currently running
org clock.

This segment overrides the modeline functionality of `org-mode-line-string'."
  (if (and (fboundp 'org-clocking-p)
             (org-clocking-p))
      (substring-no-properties (funcall spaceline-org-clock-format-function))
    "Not clocking!")
  :global-override org-mode-line-string
  :face ruin/warning-face)

(spaceline-define-segment org-clock-not
  "Show when not clocking."
  (concat "asd" "zxc")
  :enabled t
  )

(defun ruin/init-textmode-theme()
  (load-theme 'monokai t)

  (set-face-background 'default "black")
  (set-face-background 'mode-line "black")
  (set-face-foreground 'font-lock-comment-face "yellow")
  (set-face-foreground 'font-lock-comment-delimiter-face "yellow")
  (set-face-foreground 'font-lock-doc-face "magenta")
  (set-face-background 'linum "black")
  (set-face-background 'powerline-active1 "black")
  (set-face-background 'powerline-active2 "black")
  (set-face-foreground 'spaceline-flycheck-info "blue")
  (set-face-foreground 'spaceline-flycheck-warning "yellow")
  (set-face-foreground 'spaceline-flycheck-error "red")
  (set-face-background 'spaceline-flycheck-info "black")
  (set-face-background 'spaceline-flycheck-warning "black")
  (set-face-background 'spaceline-flycheck-error "black")
  (set-face-background 'helm-selection "blue"))

(defun ruin/classic-theme ()
  (set-frame-font "SGI Screen:style=Regular:pixelsize=14" t)
  (load-theme 'the-stars t)
  ;; (scroll-bar-mode)
  ;; (tool-bar-mode)
  ;; (menu-bar-mode)
  )

(defun ruin/normal-theme ()
  (interactive)
  (load-theme 'monokai t))

(defun ruin/growth-theme ()
  (interactive)
  (when (eq system-type 'windows-nt)
    (set-frame-font "y-outline-ＭＳ ゴシック-normal-normal-normal-mono-14-*-*-*-c-*-iso10646-1"))
  (load-theme 'consonance t)
  (transparency 95)
  (toggle-frame-fullscreen))

(defun ruin/init-theme ()
  (setup-cjk-alignment)
  (spaceline-compile)
  (spaceline-toggle-org-clock-on)
  (spaceline-toggle-org-clock-not-on)
  (if (not window-system)
      (ruin/init-textmode-theme)
    (ruin/normal-theme))
  (spaceline-emacs-theme)
  (spaceline-helm-mode)
  (spaceline-toggle-projectile-root-on)
  (spaceline-toggle-which-function-off))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (ruin/init-theme)
                ))
  (ruin/init-theme))

(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

(package-require 'fill-column-indicator)
(setq fill-column 80)
(fci-mode)

(defun ruin/enable-filling ()
  (interactive)
  ; for Rust
  (setq fill-column 99)
  (auto-fill-mode)
  (fci-mode))

(add-hook 'rust-mode-hook 'ruin/enable-filling)

;; ;; Calculate default font size
;; (setq default-frame-font-size 9)
;; (setq presentation-frame-font-size
;;       (truncate (* 1.25 default-frame-font-size)))

;; ;; Build font descriptor strings
;; ;; (defun font-desc (name size)
;; ;;   (concat "-unknown-" name "-normal-normal-normal-*-"
;; ;;           (number-to-string size) "-*-*-*-m-0-iso10646-1"))
;; (defun font-desc (name size)
;;   (concat name " " (number-to-string size)))

;; ;; Set default and presentation mode fonts
;; (defun default-frame-font ()
;;   (font-desc "Menlo for Powerline" default-frame-font-size))
;; (defun presentation-frame-font ()
;;   (font-desc "Menlo for Powerline" presentation-frame-font-size))
;; (set-frame-font (default-frame-font))
;; (add-to-list 'default-frame-alist '(font . "Menlo\ for\ Powerline-9"))

;; (require 'iosevka-ligatures)

(provide 'ruin-theme)
