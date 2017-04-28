(package-require 'projectile)
(package-require 'helm-projectile)

(projectile-global-mode)
(helm-projectile-on)

(setq projectile-completion-system 'helm)

(setq projectile-globally-ignored-directories
      (append projectile-globally-ignored-directories '(".git"
                                                        ".svn"
                                                        ".hg"
                                                        "elpa"
                                                        "vendor"
                                                        "bak"
                                                        "tmp"
                                                        "auto-save-list"
                                                        "semanticdb"
                                                        )))

(evil-leader/set-key
  "ps" 'helm-projectile-switch-project
  "pa" 'helm-projectile-ag
  "pf" 'helm-projectile
  "pb" 'helm-projectile-switch-to-buffer
  "p!" 'projectile-run-async-shell-command-in-root
  "pc" 'projectile-compile-project
  "pr" 'projectile-replace
  )

(defun ruin/open-project-info (&rest r)
  "Opens this projectile project's information, todos, etc.

   I like keeping this information around to stay organized."
  (interactive)
  (let ((filename (expand-file-name
                   "project.org" (projectile-project-root))))
    (when (file-exists-p filename)
      (popwin:popup-buffer (find-file-noselect filename) :position 'top :height 15 :stick t :noselect t))))

(advice-add 'helm-projectile-switch-project :after 'ruin/open-project-info)

(defun ruin/project-errors ()
  "Looks through all active buffers in this project and collects all errors."
  (interactive)
  (seq-mapcat (lambda (buffer)
              (seq-copy (buffer-local-value 'flycheck-current-errors buffer)))
            (projectile-project-buffers)))

(defun ruin/list-project-errors ()
  "Show the error list for the current project."
  (interactive)
  (let (flycheck-current-errors (ruin/project-errors))
    (flycheck-list-errors)))


;; (eval-after-load "helm" (helm-add-action-to-source "Ag in projects" 'helm-projectile-ag helm-source-projectile-projects))

(provide 'ruin-project)
