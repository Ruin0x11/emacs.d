;;; ruin-flycheck.el --- flycheck settings

(package-require 'flycheck)
(package-require 'helm-flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)

(setq flycheck-standard-error-navigation nil)

(eval-after-load "flycheck"
  '(progn
     (setq flycheck-highlighting-mode 'symbols)
     (set-face-foreground 'flycheck-warning nil)

     (define-key flycheck-error-list-mode-map "j" 'next-line)
     (define-key flycheck-error-list-mode-map "k" 'previous-line)
     (ruin/window-movement-for-map flycheck-error-list-mode-map)
    ))

(eval-after-load "flycheck"
  '(evil-leader/set-key-for-mode 'flycheck-mode
     "he" 'helm-flycheck))

(setq-default flycheck-disabled-checkers '(markdown processing))
(add-hook 'mmm-major-mode-hook (lambda () (flycheck-mode nil)))

(provide 'ruin-flycheck)
