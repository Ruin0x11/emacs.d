;;; ruin-git.el --- git/magit settings

(package-require 'magit)
(require 'magit)

(evil-leader/set-key
        "gB" 'magit-blame
        "gb" 'magit-branch
        "gS" 'magit-checkout
        "gl" 'magit-log-all
        "gL" 'magit-log-buffer-file
        "gs" 'magit-status
        "gf" 'magit-fetch-all
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

(add-to-list 'evil-insert-state-modes 'git-commit-mode)

(provide 'ruin-git)
