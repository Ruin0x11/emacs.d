;;; ruin-flycheck.el --- flycheck settings

(package-require 'flycheck)
(package-require 'helm-flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)

(setq flycheck-standard-error-navigation nil)

(eval-after-load "flycheck"
  '(progn
     (setq flycheck-highlighting-mode 'symbols
           flycheck-display-errors-delay 0.8)
     (set-face-foreground 'flycheck-warning nil)

     (define-key flycheck-error-list-mode-map "j" 'next-line)
     (define-key flycheck-error-list-mode-map "k" 'previous-line)
     (ruin/window-movement-for-map flycheck-error-list-mode-map)


(flycheck-define-checker csharp-unity
  "Custom checker for Unity projects"
  :modes (csharp-mode)
  :command ("compile-maid")
  :error-patterns((warning line-start (file-name) "(" line (zero-or-more not-newline) "): warning " (message) line-end)
                  (error line-start (file-name) "(" line (zero-or-more not-newline) "): " (message) line-end))
  :modes csharp-mode)

(flycheck-define-checker csharp-hand
  "Custom checker for Unity projects"
  :modes (csharp-mode)
  :command ("compile-hand")
  :error-patterns((warning line-start (file-name) "(" line (zero-or-more not-newline) "): warning " (message) line-end)
                  (error line-start (file-name) "(" line (zero-or-more not-newline) "): " (message) line-end))
  :modes csharp-mode)
    ))

(eval-after-load "flycheck"
  '(evil-leader/set-key-for-mode 'flycheck-mode
     "he" 'helm-flycheck))

(setq-default flycheck-disabled-checkers '(markdown processing))
(add-hook 'mmm-major-mode-hook (lambda () (flycheck-mode nil)))

(add-to-list 'flycheck-disabled-checkers 'haml)

(provide 'ruin-flycheck)
