(package-require 'smartparens)
(require 'smartparens-config)

(smartparens-global-mode t)
(add-lisp-hook 'smartparens-strict-mode)

(define-key sp-keymap (kbd "M-C-h") 'sp-backward-sexp)
(define-key sp-keymap (kbd "M-C-l") 'sp-forward-sexp)
(define-key sp-keymap (kbd "M-l") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "M-h") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "M-S-l") 'sp-backward-barf-sexp)
(define-key sp-keymap (kbd "M-S-h") 'sp-backward-slurp-sexp)

(show-smartparens-global-mode t)
