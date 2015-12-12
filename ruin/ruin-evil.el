;;; ruin-evil.el --- evil settings and non-package mappings

; (setq evil-want-C-u-scroll t)

(package-require 'evil)
(package-require 'evil-commentary)
(package-require 'evil-surround)
(package-require 'evil-leader)
(package-require 'evil-jumper)
(evil-mode 1)
(evil-jumper-mode 1)

(global-evil-leader-mode)
(global-evil-surround-mode)
(evil-commentary-mode)

;; leader binds
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "r" 'quickrun
  "R" 'quickrun-shell
  "w" 'save-buffer
  "j" 'jump-to-register
  ;; "p" 'helm-find-files
  ;; "q" 'kill-buffer-and-window
  "q" 'evil-quit
  "x" 'evil-save-and-close
  "u" 'universal-argument
  "y" 'helm-show-kill-ring

  "df" 'describe-function
  "dv" 'describe-variable
  "dm" 'describe-mode
  "dk" 'describe-key
  "dp" 'describe-package
  "dd" 'describe-foo-at-point

  "eb" 'eval-buffer
  "es" 'eval-last-sexp
  "ee" 'eval-expression
  "ed" 'eval-defun

  "a" 'org-agenda
  "s" 'org-store-link
  "c" 'org-capture
  "ob" 'org-iswitchb
  "oc" 'org-clock-goto

  "ps" 'helm-projectile-switch-project
  "pa" 'helm-projectile-ag
  "pf" 'helm-projectile-find-file
  "ph" 'helm-projectile
  "pb" 'helm-projectile-switch-to-buffer
  "p!" 'projectile-run-async-shell-command-in-root
  "pc" 'projectile-compile-project
  "pr" 'helm-projectile-recentf

  "ff" 'helm-find-files
  "fr" 'helm-recentf
  "hr" 'helm-regexp
  "hm" 'helm-man-woman
  "hb" 'helm-bookmarks
  "hi" 'helm-imenu

  "bl" 'helm-buffers-list
  "TAB" 'spacemacs/alternate-buffer
  "bd"  'kill-this-buffer
  "bD"  'spacemacs/kill-other-buffers
  "bY"  'spacemacs/copy-whole-buffer-to-clipboard
  "b!"  'spacemacs/open-in-external-app
  "b="  'my-diff-buffer-with-file
  )

;; normal mode binds
(defun copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))

(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

; (defun comint-goto-end-and-insert ()
;   (interactive)
;   (if (not (comint-after-pmark-p))
;       (progn (comint-goto-process-mark)
;              (evil-append-line nil))
;     (evil-insert 1)))

; (evil-define-key 'normal comint-mode-map "i" 'comint-goto-end-and-insert)
; (evil-define-key 'normal inf-ruby-mode-map "i" 'comint-goto-end-and-insert)

; (evil-define-key 'insert comint-mode-map
;   (kbd "<up>") 'comint-previous-input
;   (kbd "<down>") 'comint-next-input)

; (evil-define-key 'insert comint-mode-map     (kbd "RET") #'comint-send-input)

(provide 'ruin-evil)
