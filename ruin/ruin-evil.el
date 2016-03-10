;;; ruin-evil.el --- evil settings and non-package mappings

(+ (- 4 5) 1)

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

;;; leader binds
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "r" (lambda () (interactive) (save-buffer) (quickrun))
  "R" 'quickrun-shell
  "w" 'save-buffer
  "j" 'jump-to-register
  "q" 'evil-quit
  "x" 'evil-save-and-close
  "u" 'universal-argument
  "y" 'helm-show-kill-ring
  "=" 'calc-dispatch
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

  "ff" 'helm-find-files
  "fg" 'helm-do-grep-ag
  "fr" 'helm-recentf
  "fd" 'helm-semantic-or-imenu
  "hR" 'helm-regexp
  "hM" 'helm-man-woman
  "hm" 'helm-mini
  "hM" 'helm-man-woman
  "hb" 'helm-bookmarks
  "hr" 'helm-resume
  "hc" 'helm-colors
  "hg" 'helm-do-grep

  "?e" 'info-emacs-manual
  "?h" 'helm-info-emacs
  ;; "ii" 'helm-info-at-point
  ;; "il" 'helm-info-elisp

  "ii" 'other-window
  "i=" 'balance-windows
  "io" 'delete-other-windows
  "ih" 'evil-window-move-far-left
  "il" 'evil-window-move-far-right
  "ij" 'evil-window-move-very-bottom
  "ik" 'evil-window-move-very-top
  "iv" 'split-window-below
  "is" 'split-window-right
  "iV" 'split-window-below-and-focus
  "iS" 'split-window-right-and-focus
  "iu" 'winner-undo
  "iU" 'winner-redo

  "bl" 'helm-buffers-list
  "TAB" 'spacemacs/alternate-buffer
  "bd"  'kill-this-buffer
  "br"  'revert-buffer
  "bD"  'spacemacs/kill-other-buffers
  "bY"  'spacemacs/copy-whole-buffer-to-clipboard
  "b!"  'spacemacs/open-in-external-app
  "b="  'my-diff-buffer-with-file
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

;; j and k where it counts
(setq archive-mode-hook #'(lambda ()
  (define-key archive-mode-map " " 'scroll-up)
  (define-key archive-mode-map "'" 'scroll-down)
  (define-key archive-mode-map "j" 'archive-next-line)
  (define-key archive-mode-map "k" 'archive-previous-line)
  (define-key archive-mode-map "J" 'scroll-up-next-n-lines)
  (define-key archive-mode-map "K" 'scroll-down-previous-n-lines)
  (define-key archive-mode-map "l" 'quit-window)
  (define-key archive-mode-map ";" 'archive-view)))


;;; normal Emacs binds
(global-set-key (kbd "C-x |") 'align-regexp)
(global-set-key (kbd "C-x =") 'eval-region)

(global-set-key [f7] 'previous-error)
(global-set-key [f8] 'next-error)
(global-set-key [f9] 'projectile-compile-project)

;;literal Ctrl-D (EOF) in term-mode
;;http://stackoverflow.com/a/27258145/5862977
(delete 'term-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'term-mode)

(provide 'ruin-evil)
