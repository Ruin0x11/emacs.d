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

;; (eval-after-load "helm" (helm-add-action-to-source "Ag in projects" 'helm-projectile-ag helm-source-projectile-projects))


(provide 'ruin-project)
