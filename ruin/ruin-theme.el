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
(menu-bar-mode 0)
(if (or (display-graphic-p) (daemonp))
    (progn
      (scroll-bar-mode 0)
      (tool-bar-mode 0)))

;; Set frame title
(setq frame-title-format '(multiple-frames "%b" ("" invocation-name "@" system-name ": End of days." )))

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
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Install themes
;; (package-require 'ample-theme)
;; (package-require 'spacemacs-theme)
;; (package-require 'zenburn-theme)
(package-require 'solarized-theme)
(package-require 'monokai-theme)
(package-require 'spaceline)
(package-require 'helm)
(package-require 'base16-theme)
(require 'helm)
(require 'spaceline-config)

(setq custom-theme-directory (locate-user-emacs-file "themes"))
(setq custom-theme-load-path (add-to-list 'custom-theme-load-path
                                          (locate-user-emacs-file "themes/btcsb")))

(add-hook 'term-mode-hook 'my-term-mode-hook)
(defun my-term-mode-hook ()
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20611
  (setq bidi-paragraph-direction 'left-to-right))

;;; Properly align CJK org-mode tables
;; https://github.com/kuanyui/.emacs.d/blob/master/rc/rc-basic.el#L102
(defun setup-cjk-alignment ()
  (when (display-graphic-p)
    (defvar emacs-english-font "Hack"
      "The font name of English.")

    (defvar emacs-cjk-font "Kochi Gothic" "The font name for CJK.")

    (defvar emacs-font-size-pair '(12 . 14)
      "Default font size pair for (english . chinese)")

    (defvar emacs-font-size-pair-list
      '(( 5 .  6) (9 . 10) (10 . 12) (12 . 14)
        (14 . 16) (15 . 18) (16 . 16) (17 . 20)
        (19 . 22) (20 . 24) (21 . 26) (24 . 28)
        (26 . 32) (28 . 34) (30 . 36) (34 . 40))
      "This list is used to store matching (english . chinese) font-size.")

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

    ;(global-set-key (kbd "C-=") 'increase-emacs-font-size)
    ;(global-set-key (kbd "C--") 'decrease-emacs-font-size)
    ))

(setq spaceline-workspace-numbers-unicode 't)

(defface ruin/warning-face '((t (:foreground "white" :background "cadetblue")))
  "Face for a warning.")

;(spaceline-define-segment org-clock
;  "Show information about the current org clock task.  Configure
;`spaceline-org-clock-format-function' to configure. Requires a currently running
;org clock.
;
;This segment overrides the modeline functionality of `org-mode-line-string'."
;  (if (and (fboundp 'org-clocking-p)
;             (org-clocking-p))
;      (substring-no-properties (funcall spaceline-org-clock-format-function))
;    "Not clocking!")
;  :global-override org-mode-line-string
;  :face ruin/warning-face)
;
;(spaceline-define-segment org-clock-not
;  "Show when not clocking."
;  (concat "asd" "zxc")
;  :enabled t
;  )

(defun ruin/init-textmode-theme()
  (load-theme 'firebelly t)
  (package-require 'helm)
  (require 'helm-files)
  (package-require 'company)
  (require 'company)
  (global-linum-mode 1)
  (set-face-background 'default "black")
  (set-face-background 'mode-line "black")
  (set-face-foreground 'font-lock-constant-face "red")
  (set-face-foreground 'font-lock-comment-face "black")
  (set-face-bold 'font-lock-comment-face t)
  (set-face-foreground 'font-lock-comment-delimiter-face "black")
  (set-face-bold 'font-lock-comment-delimiter-face t)
  (set-face-foreground 'font-lock-variable-name-face "yellow")
  (set-face-foreground 'font-lock-function-name-face "green")

  ;(set-face-foreground 'evil-ex-lazy-highlight "black")
  ;(set-face-background 'evil-ex-lazy-highlight "red")
  (set-face-foreground 'font-lock-doc-face "magenta")
  (set-face-foreground 'linum "black")
  (set-face-bold 'linum t)
  (set-face-background 'linum "black")
  (set-face-background 'region "yellow")
  (set-face-background 'powerline-active1 "black")
  (set-face-background 'powerline-active2 "black")
  (set-face-foreground 'spaceline-flycheck-info "blue")
  (set-face-foreground 'spaceline-flycheck-warning "yellow")
  (set-face-foreground 'spaceline-flycheck-error "red")
  (set-face-background 'spaceline-flycheck-info "black")
  (set-face-background 'spaceline-flycheck-warning "black")
  (set-face-background 'spaceline-flycheck-error "black")
  (set-face-background 'helm-selection "blue")
  (set-face-foreground 'helm-ff-file "green")
  (set-face-foreground 'company-tooltip "yellow")
  (set-face-foreground 'company-tooltip-annotation "magenta")
  (set-face-foreground 'company-tooltip-common "magenta")
  (with-eval-after-load 'evil
    (set-face-background 'evil-ex-lazy-highlight "magenta"))
)

(defun ruin/classic-theme ()
  (set-frame-font "SGI Screen:style=Regular:pixelsize=14" t)
  (load-theme 'the-stars t))

(defun ruin/normal-theme ()
  (interactive)
  (load-theme 'monokai t)
  (toggle-frame-fullscreen))

(defun ruin/classic-theme-windows ()
  (when (eq system-type 'windows-nt)
    (set-frame-font "y-outline-MS Gothic-normal-normal-normal-mono-13-*-*-*-c-*-iso10646-1"))
  (when (eq system-type 'gnu/linux)
    (set-frame-font "-*-terminus-medium-*-*-*-12-*-*-*-*-*-*"))
  ;(load-theme 'undy t)
  ;(load-theme 'base16-hopscotch t)
  ;(load-theme 'base16-atelier-savanna t)
  (require 'moe-theme)
  (setq moe-theme-mode-line-color 'w/b)
  (moe-dark)
  (transparency 100)
  (set-frame-size (selected-frame) 140 80)
  (setq flycheck-color-mode-line-face-to-color 'mode-line-buffer-id))

(defun ruin/growth-theme ()
  (interactive)
  (when (eq system-type 'windows-nt)
    (set-frame-font "y-outline-ＭＳ ゴシック-normal-normal-normal-mono-20-*-*-*-c-*-iso10646-1"))
  (load-theme 'consonance t)
  (transparency 95)
  (toggle-frame-fullscreen))

(defun ruin/init-spaceline ()
  (spaceline-compile)
  ;; (spaceline-toggle-org-clock-on)
  ;; (spaceline-toggle-org-clock-not-on)
  (spaceline-emacs-theme)
  (spaceline-helm-mode)
  (spaceline-toggle-projectile-root-on)
  (spaceline-toggle-which-function-off)
  (setq flycheck-color-mode-line-face-to-color 'powerline-active1))

(defun ruin/init-theme ()
  (interactive)
  (setup-cjk-alignment)
  (display-battery-mode)

  ; (when (window-system)
  ;   (ruin/init-spaceline))

  (cond ((not window-system) (ruin/init-textmode-theme))
        ((memq system-type '(darwin)) (ruin/normal-theme))
        (t (ruin/classic-theme-windows))))

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
;(fci-mode)

(defun ruin/enable-filling ()
  (interactive)
  ; for Rust
  (setq fill-column 80)
  (auto-fill-mode)
  (fci-mode 0))

(add-hook 'rust-mode-hook 'ruin/enable-filling)

(defun ruin/battery-pmset ()
  "Get battery status information using `pmset'.

The following %-sequences are provided:
%L Power source (verbose)
%B Battery status (verbose)
%b Battery status, empty means high, `-' means low,
   `!' means critical, and `+' means charging
%p Battery load percentage
%h Remaining time in hours
%m Remaining time in minutes
%t Remaining time in the form `h:min'"
  (let (power-source load-percentage battery-status battery-status-symbol
	remaining-time hours minutes)
    (with-temp-buffer
      (ignore-errors (call-process "pmset" nil t nil "-g" "ps"))
      (goto-char (point-min))
      (when (re-search-forward "\\(?:Currentl?y\\|Now\\) drawing from '\\(AC\\|Battery\\) Power'" nil t)
	(setq power-source (match-string 1))
	(when (re-search-forward "^ -InternalBattery-0 \(id=\\([0-9]*\\))[ \t]+" nil t)
	  (when (looking-at "\\([0-9]\\{1,3\\}\\)%")
	    (setq load-percentage (match-string 1))
	    (goto-char (match-end 0))
	    (cond ((looking-at "; charging")
		   (setq battery-status "charging"
			 battery-status-symbol "+"))
		  ((< (string-to-number load-percentage) battery-load-critical)
		   (setq battery-status "critical"
			 battery-status-symbol "!"))
		  ((< (string-to-number load-percentage) battery-load-low)
		   (setq battery-status "low"
			 battery-status-symbol "-"))
		  (t
		   (setq battery-status "high"
			 battery-status-symbol "")))
	    (when (re-search-forward "\\(\\([0-9]+\\):\\([0-9]+\\)\\) remaining"  nil t)
	      (setq remaining-time (match-string 1))
	      (let ((h (string-to-number (match-string 2)))
		    (m (string-to-number (match-string 3))))
		(setq hours (number-to-string (+ h (if (< m 30) 0 1)))
		      minutes (number-to-string (+ (* h 60) m)))))))))
    (list (cons ?L (or power-source "N/A"))
	  (cons ?p (or load-percentage "N/A"))
	  (cons ?B (or battery-status "N/A"))
	  (cons ?b (or battery-status-symbol ""))
	  (cons ?h (or hours "N/A"))
	  (cons ?m (or minutes "N/A"))
	  (cons ?t (or remaining-time "N/A")))))

(when (memq system-type '(darwin))
  (setq battery-status-function #'ruin/battery-pmset))

(provide 'ruin-theme)
