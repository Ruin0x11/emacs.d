;; init.el -- inits

;; Add .emacs.d/ruin to load-path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "ruin"))

;; Set paths to custom.el and loaddefs.el
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(require 'cl)
(defun online? ()
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

;; ELPA
(setq package-user-dir (concat dotfiles-dir "elpa"))
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

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir "bak")))))

;; Macro for X specific code
(defmacro Xlaunch (&rest x)
  (list 'if (eq window-system 'x) (cons 'progn x)))

(setq evil-want-C-u-scroll t)

;; modularize separate features
(setq ruin-pkg
      '(linum-off
        ruin-powerline
        ruin-theme

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
        ruin-html
        ruin-haskell
        ruin-go
        ruin-tex

        ruin-project
        ruin-flycheck
        ruin-snippet
        ruin-git
        ruin-mail
        ruin-shell
        ruin-misc-modes
        ruin-home
        ))

;; load modularized features
(dolist (file ruin-pkg)
  (require file))

(load custom-file 'noerror)
(put 'downcase-region 'disabled nil)
