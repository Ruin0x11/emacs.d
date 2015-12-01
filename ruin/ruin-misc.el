;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Automatically save buffers before compiling
(setq compilation-ask-about-save nil)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Transparently open compressed files
(auto-compression-mode t)

;; Save a list of recent files visited.
(recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; default unified diffs
(setq diff-switches "-u")

(setq gdb-many-windows t)

;; registers
(dolist
    (r `((?i (file . ,(concat dotfiles-dir "init.el")))
         (?o (file . ,(expand-file-name "~/Dropbox/org/")))
         (?r (file . ,(let* ((user user-login-name)
                             (org (expand-file-name (concat user ".org") dotfiles-dir))
                             (el  (expand-file-name (concat user ".el") dotfiles-dir))
                             (dir (expand-file-name user dotfiles-dir)))
                        (cond
                         ((file-exists-p org) org)
                         ((file-exists-p el)  el)
                         (t dir)))))
         ))
  (set-register (car r) (cadr r)))

(provide 'ruin-misc)
