(package-require 'projectile)
(package-require 'helm-projectile)
(package-require 'helm-rg)

(require 'projectile)

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
                                                        "build"
                                                        "deps"
                                                        "CMakeFiles"
                                                        "external"
                                                        )))

(setq projectile-globally-ignored-file-suffixes '("class" "db" "min.css"))

(evil-leader/set-key
  "ps" 'helm-projectile-switch-project
  "pa" 'helm-projectile-ag
  "pf" 'helm-projectile
  "pb" 'helm-projectile-switch-to-buffer
  "p!" 'projectile-run-async-shell-command-in-root
  "pc" 'projectile-compile-project
  "pr" 'projectile-replace
  "pR" 'projectile-replace-regexp
  "pu" 'projectile-run-project
  "pi" 'ruin/open-project-info
  "pt" 'projectile-test-project
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

(defun ruin/projectile-compile-project ()
  "Compile project using last projectile compile command."
  (interactive)
  (let ((command (projectile-compilation-command (projectile-compilation-dir)))
        (compilation-read-command nil))
    (projectile--run-project-cmd command projectile-compilation-cmd-map)))

(defun ruin/projectile-run-project ()
  "Compile project using last projectile run command."
  (interactive)
  (let ((command (projectile-run-command (projectile-compilation-dir)))
        (compilation-read-command nil))
    (projectile--run-project-cmd command projectile-run-cmd-map)))

(defun ruin/elobuild ()
  (interactive)
  (let ((compilation-read-command nil))
    (projectile--run-project-cmd "elocopy && elobuild" projectile-compilation-cmd-map)))

(defun ruin/elobuild-and-run ()
  (interactive)
  (let ((compilation-read-command nil))
    (projectile--run-project-cmd "elocopy && elobuild && cd bin && ./Elona_foobar" projectile-compilation-cmd-map)))

(defun ruin/elobuild-test (test-name)
  (interactive "stest? ")
  (let ((cmd (if (eq (length test-name) 0)
                 "elocopy && elobuild test"
               (concat "elocopy && elobuild test \"" test-name "\"")))
        (compilation-read-command nil))
    (projectile--run-project-cmd cmd projectile-compilation-cmd-map)))

(defun ruin/run-elona ()
  (interactive)
  (let ((compilation-read-command nil))
    (projectile--run-project-cmd "elocopy && cd bin && ./Elona_foobar" projectile-run-cmd-map)))

(defun ruin/make-clean ()
  (interactive)
  (let ((compilation-read-command nil))
    (projectile--run-project-cmd "make clean" projectile-compilation-cmd-map)))

(global-set-key [f9] 'ruin/elobuild-test)
(global-set-key [f10] 'ruin/elobuild)
(global-set-key (kbd "S-<f10>") 'ruin/elobuild-and-run)
(global-set-key [f11] 'ruin/make-clean)
(global-set-key [f12] 'ruin/run-elona)

;; (eval-after-load "helm" (helm-add-action-to-source "Ag in projects" 'helm-projectile-ag helm-source-projectile-projects))

(provide 'ruin-project)
