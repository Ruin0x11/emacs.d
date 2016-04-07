;;; ruin-company.el --- company setup

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-n") 'company-select-next)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)
     (define-key company-active-map (kbd "TAB") 'company-complete)
     (define-key company-active-map [tab] 'company-complete)
     ))

(package-require 'company)

(global-company-mode)

(eval-after-load 'company
  '(push 'company-semantic company-backends))

(provide 'ruin-company)
