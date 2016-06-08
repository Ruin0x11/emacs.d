(package-require 'smartparens)
(package-require 'evil-smartparens)
(require 'smartparens-config)
(require 'cl)

(setq lisp-modes
      '(scheme-mode emacs-lisp-mode lisp-mode clojure-mode
                    lisp-interaction-mode))

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

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "eb" 'ruin/write-and-eval-buffer
  "es" 'eval-last-sexp
  "eh" 'helm-eval-expression-with-eldoc
  "ed" 'eval-defun
  )

(evil-leader/set-key-for-mode 'lisp-interaction-mode
  "eb" 'ruin/write-and-eval-buffer
  "es" 'eval-last-sexp
  "eh" 'helm-eval-expression-with-eldoc
  "ed" 'eval-defun
  )


(define-key smartparens-mode-map (kbd "M-l") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "M-h") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "M-j") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "M-k") 'sp-previous-sexp)
(define-key smartparens-mode-map (kbd "C-s") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-s") 'sp-forward-barf-sexp)

(show-smartparens-global-mode t)

(eval-after-load "ielm" #'(lambda ()
                             (ruin/window-movement-for-map inferior-emacs-lisp-mode-map)
                             (define-key inferior-emacs-lisp-mode-map (kbd "<down>") 'comint-next-input)
                             (define-key inferior-emacs-lisp-mode-map (kbd "<up>") 'comint-previous-input)))
(add-to-list 'evil-emacs-state-modes 'inferior-emacs-lisp-mode)

;; Clojure
(package-require 'cider)
(package-require 'cider-eval-sexp-fu)
;;(package-require 'clj-refactor)
(require 'cider-eval-sexp-fu)

(eval-after-load "cider" #'(lambda ()
                             (ruin/window-movement-for-map cider-repl-mode-map)
                             (ruin/window-movement-for-map cider-docview-mode-map)
                             (ruin/window-movement-for-map cider-stacktrace-mode-map)
                             (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)
                             (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)))
(add-to-list 'evil-emacs-state-modes 'cider-repl-mode)
(add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)
(add-to-list 'evil-emacs-state-modes 'cider-docview-mode)

(evil-leader/set-key-for-mode 'clojure-mode
  "mj" 'cider-jack-in
  "ma" 'cider-apropos-documentation
  "eb" 'cider-eval-buffer
  "ee" 'cider-read-and-eval
  "es" 'cider-eval-last-sexp
  "ed" 'cider-eval-defun-at-point
  "ei" 'cider-switch-to-repl-buffer

  "tt" 'cider-test-run-test
  "tn" 'cider-test-run-ns-tests
  "ta" 'cider-test-run-project-tests
  "td" 'cider-test-ediff
  "tb" 'cider-test-show-report
  "tr" 'cider-test-rerun-tests

  ;; "df" 'describe-function
  ;; "dv" 'describe-variable
  ;; "dm" 'describe-mode
  ;; "dk" 'describe-key
  "dd" 'cider-doc
  ;; "da" 'helm-apropos
  )

(setq cider-show-error-buffer 'except-in-repl
      cider-prompt-for-symbol nil)

(evil-define-key 'normal clojure-mode-map (kbd "<C-S-return>") 'cider-eval-defun-at-point)
(evil-define-key 'insert clojure-mode-map (kbd "<C-S-return>") 'cider-eval-defun-at-point)

(evil-define-key 'normal clojure-mode-map (kbd "<C-return>") 'cider-eval-last-sexp)
(evil-define-key 'insert clojure-mode-map (kbd "<C-return>") 'cider-eval-last-sexp)
(evil-define-key 'visual clojure-mode-map (kbd "<C-return>") 'cider-eval-region)
(provide 'ruin-lisp)
