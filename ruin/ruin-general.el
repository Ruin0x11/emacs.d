;;; ruin-general.el --- miscellaneous packages and settings
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
(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq recentf-max-saved-items 1000)
(run-with-idle-timer 30 t (lambda () (let ((inhibit-message t)) (recentf-save-list))))
; (recentf-cleanup)

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

;; open is generic url browser
(setq browse-url-generic-program "open")

;; registers
(dolist
    (r `((?i (file . ,(locate-user-emacs-file "init.el")))
         (?o (file . ,(expand-file-name "~/Dropbox/org/")))
         (?r (file . ,(let* ((user user-login-name)
                             (org (expand-file-name (concat user ".org") dotfiles-dir))
                             (el  (expand-file-name (concat user ".el") dotfiles-dir))
                             (dir (expand-file-name user dotfiles-dir)))
                        (cond
                         ((file-exists-p org) org)
                         ((file-exists-p el)  el)
                         (t dir)))))
         ))
  (set-register (car r) (cadr r)))

;; all buffers, try to reuse windows across all frames
(add-to-list 'display-buffer-alist
           '(".*". (display-buffer-reuse-window .
                                  ((reusable-frames . t)))))

;; except for compilation buffers where you want new and dedicated frames when necessary
(add-to-list 'display-buffer-alist
         '("^\\*Compile-Log\\*". ((display-buffer-reuse-window
                                   display-buffer-pop-up-frame) .
                                  ((reusable-frames . t)
                                  (inhibit-same-window . t)))))


(defvar truncated-compilation-line-limit 1000)
(defvar truncated-compilation-line-trailer "…")

(defun truncate-compilation-long-lines ()
  "Emacs doesn't cope well with extremely long
lines. Unfortunately some processes like grep, ack, ag, rg are
prone to matching minified files or otherwise extremely long
lines. Once Added to compilation-filter-hook, this function
truncates lines returned by the compilation process."
  (cl-flet ((truncate-line (pos)
                           (let* ((beginning (progn (beginning-of-line) (point)))
                                  (ending (progn (end-of-line) (point)))
                                  (length (- ending beginning))
                                  (excess (max (- length truncated-compilation-line-limit))))
                             (when (plusp excess)
                               (delete-region (- ending excess) ending)
                               (when truncated-compilation-line-trailer
                                 (insert truncated-compilation-line-trailer))))))
    (goto-char compilation-filter-start)
    (cl-loop do (truncate-line (point))
             (forward-line 1)
             (end-of-line)
             (when (= (point) (point-max))
               (return)))))


(add-hook 'compilation-filter-hook 'truncate-compilation-long-lines)

(when (eq system-type 'windows-nt)
  ;(setq comint-eol-on-send t)
  (setq comint-redirect-verbose t)
  (setq comint-redirect-echo-input t)
  (shell-command "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Auxiliary\\Build\\vcvars64.bat"))

(setq kill-ring-max 500)

(provide 'ruin-general)
