(package-require 'smartparens)
(require 'smartparens-config)

(smartparens-global-mode t)
; (add-lisp-hook 'smartparens-strict-mode)

(defmacro def-pairs (pairs)
  `(progn
     ,@(loop for (key . val) in pairs
          collect
            `(defun ,(read (concat
                            "wrap-with-"
                            (prin1-to-string key)
                            "s"))
                 (&optional arg)
               (interactive "p")
               (sp-wrap-with-pair ,val)))))

(def-pairs ((paren        . "(")
            (bracket      . "[")
            (brace        . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote   . "`")))

(define-key smartparens-mode-map (kbd "M-b") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "M-e") 'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "M-j") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "M-k") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-j") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "M-l") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "M-h") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "C-M-j") 'sp-beginning-of-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") 'sp-beginning-of-previous-sexp)
(define-key smartparens-mode-map (kbd "C-M-l") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "C-M-h") 'sp-backward-symbol)
(define-key smartparens-mode-map (kbd "M-[") 'sp-backward-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-]") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-d") 'sp-kill-sexp)

(define-key smartparens-mode-map (kbd "C-c (") 'wrap-with-parens)
(define-key smartparens-mode-map (kbd "C-c {") 'wrap-with-braces)

(show-smartparens-global-mode t)
(provide 'ruin-lisp)
