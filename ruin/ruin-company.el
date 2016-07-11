;; ruin-company.el --- company setup

(defun bw/company-complete-lambda (arg)
  "Ignores passed in arg like a lambda and runs company-complete"
  (company-complete))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-n") 'company-select-next)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)
     (define-key company-active-map (kbd "TAB") 'company-complete)
     (define-key company-active-map [tab] 'company-complete)
     (define-key company-active-map (kbd "<tab>") 'company-complete)

     (setq
      ;; never start auto-completion unless I ask for it
      company-idle-delay nil
      ;; autocomplete right after '.'
      company-minimum-prefix-length 0
      ;; remove echo delay
      company-echo-delay 0
      ;; don't complete in certain modes
      company-global-modes '(not git-commit-mode)
      company-dabbrev-downcase nil
      ;; make sure evil uses the right completion functions
      evil-complete-next-func 'bw/company-complete-lambda
      evil-complete-previous-func 'bw/company-complete-lambda)
     ))

(package-require 'company)

(global-company-mode)

(evil-define-key 'insert company-active-map (kbd "TAB") 'company-complete)

(eval-after-load 'company
  '(push 'company-semantic company-backends))

(provide 'ruin-company)
