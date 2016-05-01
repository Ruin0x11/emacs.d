;;; ruin-flycheck.el --- flycheck settings

(package-require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

(eval-after-load "flycheck"
  '(progn
     (setq flycheck-highlighting-mode 'symbols)
     (set-face-foreground 'flycheck-warning nil)

     (define-key flycheck-error-list-mode-map "j" 'next-line)
     (define-key flycheck-error-list-mode-map "k" 'previous-line)
     (ruin/window-movement-for-map flycheck-error-list-mode-map)
    ))

(setq-default flycheck-disabled-checkers '(processing))

(provide 'ruin-flycheck)
