;;; ruin-company.el --- company setup

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-select-next)
     (define-key company-active-map [tab] 'company-select-next)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
     (define-key company-active-map [S-tab] 'company-select-previous)
     ))

(package-require 'company)

(global-company-mode)

(eval-after-load 'company
  '(push 'company-semantic company-backends))

(provide 'ruin-company)
