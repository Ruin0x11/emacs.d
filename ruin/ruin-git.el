;;; ruin-git.el --- git/magit settings

(package-require 'magit)
(package-require 'evil-magit)
(package-require 'git-timemachine)
(package-require 'smeargle)
(package-require 'gitignore-mode)
(package-require 'magithub)
(require 'magit)
(require 'evil-magit)
(evil-magit-init)

(if (eq system-type 'windows-nt)
    (progn
      (setq exec-path (add-to-list 'exec-path "C:/Program Files/Git/bin"))
      (setenv "PATH" (concat "C:\\Program Files\\Git\\bin;" (getenv "PATH")))))


(add-to-list 'evil-emacs-state-modes 'git-timemachine-mode)

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

(defun ruin/navigate-merge-conflicts ()
  (interactive)
  (grep (concat "grep -nH -r -e \"<<<<<<< HEAD\" " (projectile-project-root))))

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
  "gv" 'endless/visit-pull-request-url
  "gn" 'ruin/navigate-merge-conflicts)

(defun magit-diff-head ()
        "Execute `magit-diff' against current HEAD."
        (interactive)
        (magit-diff "HEAD"))

;; always open symlinks as actual file
(setq vc-follow-symlinks t
      magit-commit-show-diff t
      magit-no-confirm '(stage-all-changes))

(add-to-list 'evil-insert-state-modes 'git-commit-mode)

(evil-leader/set-key-for-mode 'smerge-mode
  "mss" 'smerge-keep-current
  "msm" 'smerge-keep-mine
  "mst" 'smerge-keep-other
  "msa" 'smerge-keep-all
  "msn" 'smerge-next
  "msp" 'smerge-prev
  "mse" 'smerge-ediff)

(general-define-key
 :keymaps '(magit-log-mode-map magit-mode-map)
 :states '(emacs)
 "C-d" 'evil-scroll-down
 "C-u" 'evil-scroll-up)


;;; magithub
(require 'magithub)
(magithub-feature-autoinject t)

(provide 'ruin-git)
