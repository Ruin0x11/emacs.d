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
     (define-key company-active-map (kbd "<escape>") (lambda () (interactive) (company-abort) (evil-normal-state)))

     (setq
      ;; always start auto-completion
      company-idle-delay nil
      ;; autocomplete right after '.'
      company-minimum-prefix-length nil
      ;; remove echo delay
      company-echo-delay 0
      company-dabbrev-downcase nil
      ;; make sure evil uses the right completion functions
      evil-complete-next-func 'bw/company-complete-lambda
      evil-complete-previous-func 'bw/company-complete-lambda)

     (setq company-global-modes '(clojurescript-mode clojure-mode
                                                     cider-repl-mode
                                                     cider-mode
                                                     ruby-mode
                                                     html-mode
                                                     css-mode
                                                     javascript-mode
                                                     emacs-lisp-mode
                                                     rust-mode
                                                     common-lisp-mode
                                                     java-mode
                                                     kotlin-mode
                                                     semantic-mode
                                                     enh-ruby-mode
                                                     ruby-mode
                                                     robe-mode
                                                     elixir-mode
                                                     alchemist-iex-mode))
     )
  )

(package-require 'company)
(package-require 'company-lsp)
(require 'company)
(push 'company-lsp company-backends)

(global-company-mode)

(evil-define-key 'insert company-active-map (kbd "TAB") 'company-complete)

; (eval-after-load 'company
;   '(push 'company-files company-backends)
;   '(push 'company-semantic company-backends))

(provide 'ruin-company)
