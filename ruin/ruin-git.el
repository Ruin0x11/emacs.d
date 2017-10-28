;;; ruin-git.el --- git/magit settings

(package-require 'magit)
(package-require 'evil-magit)
(package-require 'git-timemachine)
(package-require 'smeargle)
(package-require 'gitignore-mode)
(require 'magit)

(setq magit-remote-add-set-remote.pushDefault "origin")

(add-to-list 'evil-emacs-state-modes 'git-timemachine-mode)

(defun ruin/magit-rebase-interactive-preserve-merge ()
  (interactive)
  (magit-rebase-interactive )
  )

(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))

(evil-leader/set-key
  "gg" 'magit-dispatch-popup
  "gB" 'magit-blame
  "gb" 'magit-branch
  "gh" 'magit-checkout
  "gH" 'magit-branch-and-checkout
  "gl" 'magit-log-all
  "gL" 'magit-log-buffer-file
  "gs" 'magit-status
  "gS" 'smeargle
  "gtt" 'magit-stash
  "gtp" 'magit-stash-pop
  "gtl" 'magit-stash-list
  "gT" 'git-timemachine
  "gf" 'magit-fetch-all
  "gM" 'magit-merge
  "gd" 'magit-diff
  "gD" 'magit-diff-head
  "gp" 'magit-push-current
  "gP" 'magit-push
  "gU" 'magit-pull-from-upstream
  "gu" 'magit-pull
  "gC" 'magit-clone
  "gc" 'magit-commit
  "gR" 'magit-reset-hard
  "gv" 'endless/visit-pull-request-url)

(defun magit-diff-head ()
        "Execute `magit-diff' against current HEAD."
        (interactive)
        (magit-diff "HEAD"))

;; always open symlinks as actual file
(setq vc-follow-symlinks t
      magit-commit-show-diff nil
      magit-remote-add-set-remote.pushDefault "origin")

(add-to-list 'evil-insert-state-modes 'git-commit-mode)

(evil-leader/set-key-for-mode 'smerge-mode
  "mss" 'smerge-keep-current
  "msm" 'smerge-keep-mine
  "mst" 'smerge-keep-other
  "msa" 'smerge-keep-all
  "msn" 'smerge-next
  "msp" 'smerge-prev
  "mse" 'smerge-ediff)

(provide 'ruin-git)
