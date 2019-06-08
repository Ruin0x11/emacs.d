;;; ruin-flycheck.el --- flycheck settings

(package-require 'flycheck)
(package-require 'helm-flycheck)
(package-require 'flycheck-color-mode-line)

;(add-hook 'after-init-hook #'global-flycheck-mode)

(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)

(setq flycheck-standard-error-navigation nil)

(eval-after-load "flycheck"
  '(progn
     (require 'flycheck)
     (setq flycheck-highlighting-mode 'symbols
           flycheck-display-errors-delay 0.8
           flycheck-idle-change-delay 3

           flycheck-check-syntax-automatically '(idle-change idle-buffer-switch)
           flycheck-buffer-switch-check-intermediate-buffers t
           flycheck-idle-change-delay 0.5
           flycheck-idle-buffer-switch-delay 0.5)

     (set-face-foreground 'flycheck-warning nil)

     (define-key flycheck-error-list-mode-map "j" 'next-line)
     (define-key flycheck-error-list-mode-map "k" 'previous-line)
     (flycheck-package-setup)
     (ruin/window-movement-for-map flycheck-error-list-mode-map)

     (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

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
(add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
(add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)

(provide 'ruin-flycheck)
