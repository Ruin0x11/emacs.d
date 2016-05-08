(package-require 'smartparens)
(package-require 'evil-smartparens)
(require 'smartparens-config)
(require 'cl)

(setq lisp-modes
      '(scheme-mode emacs-lisp-mode lisp-mode clojure-mode
                    lolisp-mode shen-mode bodol-mode))

(defun add-lisp-hook (func)
  (add-hooks lisp-modes func))

(smartparens-global-mode t)
(add-lisp-hook 'smartparens-strict-mode)
(add-lisp-hook #'evil-smartparens-mode)
(add-lisp-hook 'eldoc-mode)

(defun turn-on-sp-navigate-consider-stringlike ()
  (unless (memq major-mode sp-navigate-consider-stringlike-sexp)
    (add-to-list 'sp-navigate-consider-stringlike-sexp major-mode)))

(setq
 sp-cancel-autoskip-on-backward-movement nil
 sp-autoskip-closing-pair 'always)

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
(define-key smartparens-mode-map (kbd "M-j") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "M-k") 'sp-previous-sexp)
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
(define-key smartparens-mode-map (kbd "C-s") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-s") 'sp-forward-barf-sexp)

(define-key smartparens-mode-map (kbd "C-c (") 'wrap-with-parens)
(define-key smartparens-mode-map (kbd "C-c {") 'wrap-with-braces)

(show-smartparens-global-mode t)


(eval-after-load "ielm" #'(lambda ()
                             (ruin/window-movement-for-map inferior-emacs-lisp-mode-map)
                             (define-key inferior-emacs-lisp-mode-map (kbd "<down>") 'comint-next-input)
                             (define-key inferior-emacs-lisp-mode-map (kbd "<up>") 'comint-previous-input)))
(add-to-list 'evil-emacs-state-modes 'inferior-emacs-lisp-mode)

;; Clojure
(package-require 'cider)
(package-require 'cider-eval-sexp-fu)
(require 'cider-eval-sexp-fu)

(eval-after-load "cider" #'(lambda ()
                             (ruin/window-movement-for-map cider-repl-mode-map)
                             (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)
                             (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)))
(add-to-list 'evil-emacs-state-modes 'cider-repl-mode)
(add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)

(evil-leader/set-key
  "mj" 'cider-jack-in
  "ma" 'cider-apropos-documentation)

(evil-leader/set-key-for-mode 'clojure-mode
  "eb" 'cider-eval-buffer
  "es" 'cider-eval-last-sexp-to-repl
  "ee" 'cider-eval-last-sexp
  "ed" 'cider-eval-defun-at-point
  "eD" 'toggle-debug-on-error
  "ei" 'cider-switch-to-repl-buffer
  )

(setq cider-show-error-buffer 'except-in-repl)

(evil-define-key 'normal clojure-mode-map (kbd "<C-S-return>") 'cider-eval-defun-at-point)
(evil-define-key 'insert clojure-mode-map (kbd "<C-S-return>") 'cider-eval-defun-at-point)

(evil-define-key 'normal clojure-mode-map (kbd "<C-return>") 'cider-eval-last-sexp)
(evil-define-key 'insert clojure-mode-map (kbd "<C-return>") 'cider-eval-last-sexp)
(evil-define-key 'visual clojure-mode-map (kbd "<C-return>") 'cider-eval-region)
(provide 'ruin-lisp)
