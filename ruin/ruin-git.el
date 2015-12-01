(package-require 'magit)
(require 'magit)

(evil-leader/set-key
        "gb" 'magit-blame
        "gl" 'magit-log-all
        "gL" 'magit-log-buffer-file
        "gs" 'magit-status
        "gd" 'magit-diff-head
        "gC" 'magit-commit)

(defun magit-diff-head ()
        "Execute `magit-diff' against current HEAD."
        (interactive)
        (magit-diff "HEAD"))

(provide 'ruin-git)
