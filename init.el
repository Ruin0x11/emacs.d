;; init.el -- inits
(require 'cl)

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
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(when (online?)
  (unless package-archive-contents (package-refresh-contents)))

(defun package-require (pkg)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

;; modularize separate features
(setq ruin-pkg
      '(ruin-theme

        ruin-general
        ruin-funcs
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
        ruin-haskell
        ruin-go
        ruin-tex
        ruin-hipchat

        ruin-project
        ruin-flycheck
        ruin-snippet
        ruin-git
        ruin-mail
        ruin-shell
        ruin-popwin
        ruin-misc-modes
        ; ruin-home
        ))

;; load modularized features
(dolist (file ruin-pkg)
  (require file))

(load custom-file 'noerror)
(put 'downcase-region 'disabled nil)
