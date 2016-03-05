;;; ruin-evil.el --- evil settings and non-package mappings

(setq evil-want-C-u-scroll t)

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
(winner-mode)

;;; leader binds
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "r" (lambda () (interactive) (save-buffer) (quickrun))
  "R" 'quickrun-shell
  "w" 'save-buffer
  "j" 'jump-to-register
  ;; "p" 'helm-find-files
  ;; "q" 'kill-buffer-and-window
  "q" 'evil-quit
  "x" 'evil-save-and-close
  "u" 'universal-argument
  "y" 'helm-show-kill-ring
  "!" 'shell-command

  "df" 'describe-function
  "dv" 'describe-variable
  "dm" 'describe-mode
  "dk" 'describe-key
  "dd" 'describe-foo-at-point
  "da" 'helm-apropos

  "eb" 'eval-buffer
  "es" 'eval-last-sexp
  "ee" 'eval-expression
  "ed" 'eval-defun

  "a" 'org-agenda
  "c" 'org-capture
  "ob" 'org-iswitchb
  "oc" 'org-clock-goto

  "ps" 'helm-projectile-switch-project
  "pa" 'helm-projectile-ag
  "pf" 'helm-projectile
  "pb" 'helm-projectile-switch-to-buffer
  "p!" 'projectile-run-async-shell-command-in-root
  "pc" 'projectile-compile-project
  "pr" 'projectile-replace

  "ff" 'helm-find-files
  "fg" 'helm-do-grep-ag
  "fr" 'helm-recentf
  "hs" 'helm-semantic-or-imenu
  "hR" 'helm-regexp
  "hM" 'helm-man-woman
  "hm" 'helm-mini
  "hM" 'helm-man-woman
  "hb" 'helm-bookmarks
  "hr" 'helm-resume
  "hc" 'helm-colors

  "ie" 'helm-info-emacs
  "ii" 'helm-info-at-point
  "il" 'helm-info-elisp

  "bl" 'helm-buffers-list
  "TAB" 'spacemacs/alternate-buffer
  "bd"  'kill-this-buffer
  "br"  'revert-buffer
  "bD"  'spacemacs/kill-other-buffers
  "bY"  'spacemacs/copy-whole-buffer-to-clipboard
  "b!"  'spacemacs/open-in-external-app
  "b="  'my-diff-buffer-with-file

  "["   'winner-undo
  "]"   'winner-redo
  )

;;; mode-based binds
(defun copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))

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
(setq sentence-end-double-space nil)

(eval-after-load "evil"
  '(progn
     (define-key evil-normal-state-map "Y" 'copy-to-end-of-line)

     ;; trade ctrl-h and others for faster window switching
     (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
     (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
     (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
     (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

     ;; global escape-to-quit
     (define-key evil-normal-state-map [escape] 'keyboard-quit)
     (define-key evil-visual-state-map [escape] 'keyboard-quit)
     (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
     (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
     (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
     (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
     (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
     (global-set-key [escape] 'evil-exit-emacs-state)
     ))

;;; normal Emacs binds
(global-set-key (kbd "C-x |") 'align-regexp)
(global-set-key (kbd "C-x =") 'eval-region)

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
