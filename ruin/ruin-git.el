;;; ruin-git.el --- git/magit settings

(package-require 'magit)
(require 'magit)

(evil-leader/set-key
        "gB" 'magit-blame
        "gb" 'magit-branch
        "gS" 'magic-checkout
        "gl" 'magit-log-all
        "gL" 'magit-log-buffer-file
        "gs" 'magit-status
        "gM" 'magit-merge
        "gd" 'magit-diff-head
        "gp" 'magit-push
        "gP" 'magit-pull
        "gC" 'magit-clone
        "gc" 'magit-commit)

(defun magit-diff-head ()
        "Execute `magit-diff' against current HEAD."
        (interactive)
        (magit-diff "HEAD"))

;; always open symlinks as actual file
(setq vc-follow-symlinks t)

(provide 'ruin-git)
