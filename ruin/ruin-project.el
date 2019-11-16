(package-require 'projectile)
(package-require 'helm-projectile)
(package-require 'helm-rg)
(package-require 'find-file-in-project)

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

(setq projectile-globally-ignored-file-suffixes '("class" "db" "min.css")
      projectile-project-compilation-cmd ""
      projectile-project-run-cmd ""

      ffip-ignore-filenames '(".ccls-cache" "android/external")
      ffip-use-rust-fd t
      ivy-on-del-error-function #'ignore
      ivy-height 30)

(defun ruin/counsel-rg-project ()
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (counsel-rg nil dir)
      (message "Not in a project."))))

(evil-leader/set-key
  "ps" 'helm-projectile-switch-project
  "pa" 'ruin/counsel-rg-project
  "pA" 'rg-project
  "pf" 'find-file-in-project
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

(defun ruin/test-elona (test-name)
  (interactive "stest? ")
  (let ((cmd (if (eq (length test-name) 0)
                 "elocopy && cd bin && ./Elona_foobar"
                 (concat "elocopy && cd bin && ./Elona_foobar \"" test-name "\"")))
        (compilation-read-command nil))
    (projectile--run-project-cmd cmd projectile-run-cmd-map)))

(defun ruin/make-clean ()
  (interactive)
  (when (yes-or-no-real-p "make clean?")
    (let ((compilation-read-command nil))
      (projectile--run-project-cmd "make clean" projectile-compilation-cmd-map))))

;(global-set-key [f9] 'ruin/elobuild-test)
;(global-set-key [f10] 'ruin/elobuild)
;(global-set-key (kbd "S-<f10>") 'ruin/elobuild-and-run)
;(global-set-key (kbd "S-<f9>") 'ruin/test-elona)
;(global-set-key [f11] 'ruin/make-clean)
;(global-set-key [f12] 'ruin/run-elona)

(defun projectile-replace (&optional arg)
  "Replace literal string in project using non-regexp `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement."
  (interactive "P")
  (let* ((directory (if arg
                        (file-name-as-directory
                         (read-directory-name "Replace in directory: "))
                      (projectile-ensure-project (projectile-project-root))))
         (old-text (read-string
                    (projectile-prepend-project-name "Replace: ")
                    (projectile-symbol-or-selection-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace %s with: " old-text))))
         (files (projectile-files-with-string old-text directory)))
    (if (version< emacs-version "27")
        ;; Adapted from `tags-query-replace' for literal strings (not regexp)
        (progn
          (setq tags-loop-scan `(let ,(unless (equal old-text (downcase old-text))
                                        '((case-fold-search nil)))
                                  (if (search-forward ',old-text nil t)
                                      ;; When we find a match, move back to
                                      ;; the beginning of it so
                                      ;; perform-replace will see it.
                                      (goto-char (match-beginning 0))))
                tags-loop-operate `(perform-replace ',old-text ',new-text t nil nil
                                                    nil multi-query-replace-map))
          (tags-loop-continue (or (cons 'list files) t)))
      (progn
        (fileloop-initialize-replace old-text new-text files 'default)
        (fileloop-continue)))))

;; (eval-after-load "helm" (helm-add-action-to-source "Ag in projects" 'helm-projectile-ag helm-source-projectile-projects))

(provide 'ruin-project)
