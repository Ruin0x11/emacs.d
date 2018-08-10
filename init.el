;; init.el -- inits
(require 'cl)

(require 'server)
(when (eq system-type 'windows-nt)
  (or (server-running-p)
      (server-start)))

;; Add .emacs.d/ruin to load-path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (locate-user-emacs-file "ruin"))

(defun reload-site-lisp ()
  "Puts site-lisp and its subdirectories into load-path."
  (interactive)
  (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((dir (locate-user-emacs-file "site-lisp"))
           (default-directory dir))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)
        (normal-top-level-add-subdirs-to-load-path)))))

(reload-site-lisp)

(defun reload-init-file ()
  "Reload my init file."
  (interactive)
  (load-file user-init-file))

;; recompile all .el files
;; (byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0)

;; Set paths to custom.el and loaddefs.el
(setq autoload-file (locate-user-emacs-file "loaddefs.el"))
(setq custom-file (locate-user-emacs-file "custom.el"))

(defun online? ()
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                              (member 'up (first (last (network-interface-info
                                                        (car iface)))))))
            (network-interface-list))
    t))

;; ELPA
(setq package-user-dir (locate-user-emacs-file "elpa"))
(require 'package)
(dolist (source '(("melpa" . "http://melpa.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(when (online?)
  (unless package-archive-contents (package-refresh-contents)))

(defun package-require (pkg)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(package-require 'exec-path-from-shell)

(when (memq system-type '(darwin))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH"))

(setq create-lockfiles nil)

(setq backup-save-directory "~/.emacs.d/saves")
(when (memq system-type '(windows-nt))
  (setenv "PATH"
          (concat
           "C:\\Windows\\Microsoft.NET\\Framework64\\v4.0.30319" ";"
           "C:\\bin" ";"
           "C:\\Program Files\\Git\\usr\\bin" ";"
           (getenv "PATH")
           ))
  )

(when (not (file-exists-p backup-save-directory))
  (make-directory backup-save-directory))

(setq backup-directory-alist
      `((".*" . ,backup-save-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,backup-save-directory t)))

;; modularize separate features
(setq ruin-pkg
      '(ruin-funcs
        ruin-theme

        ruin-general
        ruin-evil
        ruin-org
        ruin-helm
        ruin-ido
        ruin-company

        ruin-lisp
        ruin-lua
        ruin-ruby
        ruin-python
        ruin-html
        ;ruin-haskell
        ;ruin-go
        ruin-c
        ruin-rust
        ;ruin-tex
        ruin-elixir
                                        ;ruin-hipchat

        ruin-project
        ruin-flycheck
        ruin-snippet
        ruin-git
        ;ruin-mail
        ruin-shell
        ruin-popwin
        ruin-misc-modes
                                        ;ruin-x11
                                        ; ruin-home
        ))

;; load modularized features
(dolist (file ruin-pkg)
  (require file))

(load custom-file 'noerror)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(defun ruin/profile-recompile ()
  (interactive)
  (push (lambda (a b) (profiler-report) (profiler-stop) (setq compilation-finish-functions nil))
        compilation-finish-functions)
  (profiler-start 'cpu)
  (recompile))

;; load sensitive configs, if available
;(let ((secrets (locate-user-emacs-file "secret.el.gpg")))
;  (when (file-exists-p secrets)
;    (ruin/load-encrypted secrets)))
