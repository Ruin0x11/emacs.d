;;; ruin-general.el --- miscellaneous packages and settings
;; Use English when working with other people
(set-language-environment "English")

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Automatically save buffers before compiling
(setq compilation-ask-about-save nil)

;; Scroll the compilation window
(setq compilation-scroll-output t)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; don't insert tabs on indent
(set-default 'indent-tabs-mode nil)

(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Transparently open compressed files
(auto-compression-mode t)

;; Save a list of recent files visited.
(recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; default unified diffs
(setq diff-switches "-u")

;; turn off cursor blinking
(blink-cursor-mode 0)

;; 4-column tabs
(setq tab-width 4)

;; don't ring the bell
(setq ring-bell-function 'ignore)

(setq gdb-many-windows t
      gdb-show-main t)

;; save clipboard before replacing
(setq save-interprogram-paste-before-kill t)

;; use primary and clipboard X selections
(setq x-select-enable-primary t)

;; autosave recentf
(run-at-time nil (* 5 60) 'recentf-save-list)

;; don't confirm on killing process
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; reduce scrolling speed
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(setq ispell-dictionary "english")

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name (locate-user-emacs-file "bak")))))

;; Write auto-save files to own directory
;; (setq auto-save-file-name-transforms
;;       `(("\\(?:[^/]*/\\)*\\(.*\\)" ,(expand-file-name (locate-user-emacs-file "auto-save/\\1")) t)))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name (locate-user-emacs-file "auto-save/\\2")) t)))

;; registers
(dolist
    (r `((?i (file . ,(locate-user-emacs-file "init.el")))
         (?o (file . ,(expand-file-name "~/Dropbox/org/")))
         (?b (file . ,(expand-file-name "~/build")))

         ))
  (set-register (car r) (cadr r)))

(provide 'ruin-general)
