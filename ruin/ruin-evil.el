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

;; leader binds
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "f" 'helm-M-x
  "r" 'helm-mini
  "w" 'save-buffer
  "j" 'jump-to-register
  "p" 'helm-find-files
  "q" 'kill-buffer-and-window
  "u" 'universal-argument
  "e" 'eval-buffer
  "m" 'helm-bookmarks

  "hf" 'describe-function
  "hv" 'describe-variable
  "hb" 'describe-binding
  "hm" 'describe-mode

  "a" 'org-agenda
  "s" 'org-store-link
  "c" 'org-capture
  "b" 'org-iswitchb
  "oc" 'org-clock-goto)




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

(provide 'ruin-evil)
