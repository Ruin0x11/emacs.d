(package-require 'smartparens)
(package-require 'evil-smartparens)
(require 'smartparens-config)
(require 'cl)

(setq lisp-modes
      '(scheme-mode emacs-lisp-mode lisp-mode clojure-mode common-lisp-mode
                    lisp-interaction-mode))

(defun add-lisp-hook (func)
  (add-hooks lisp-modes func))

(smartparens-global-mode t)
(add-lisp-hook 'smartparens-strict-mode)
;; (add-lisp-hook #'evil-smartparens-mode)
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

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

(eval-after-load "cider" #'(lambda ()
                             (ruin/window-movement-for-map cider-repl-mode-map)
                             (ruin/window-movement-for-map cider-docview-mode-map)
                             (ruin/window-movement-for-map cider-stacktrace-mode-map)
                             (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)
                             (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
                             (define-key cider-repl-mode-map (kbd "M-n") 'cider-repl-next-input)
                             (define-key cider-repl-mode-map (kbd "M-p") 'cider-repl-previous-input)
                             ))
(add-to-list 'evil-emacs-state-modes 'cider-repl-mode)
(add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)
(add-to-list 'evil-emacs-state-modes 'cider-docview-mode)
(add-to-list 'evil-emacs-state-modes 'cider-popup-buffer-mode)

(evil-leader/set-key-for-mode 'clojure-mode
  "mj" 'cider-jack-in
  "ma" 'cider-apropos-documentation
  "mr" 'cider-switch-to-repl-buffer
  "mb" 'connect-burgundy
  "eb" 'cider-eval-buffer
  "ee" 'cider-read-and-eval
  "es" 'cider-eval-last-sexp
  "ed" 'cider-eval-defun-at-point
  "eD" 'cider-pprint-eval-defun-at-point
  "en" 'cider-repl-set-ns

  "fs" 'cider-find-var

  "tt" 'cider-test-run-test
  "tn" 'cider-test-run-ns-tests
  "ta" 'cider-test-run-project-tests
  "td" 'cider-test-ediff
  "tb" 'cider-test-show-report
  "tr" 'cider-test-rerun-tests

  "dd" 'cider-doc
  )

(defun connect-burgundy ()
  (interactive)
  (start-file-process-shell-command "burgundy"
                                    (get-buffer-create "*burgundy*")
                                    "lein run")
  (sit-for 5)
  (cider-connect "localhost" 7777))

(setq cider-show-error-buffer t
      cider-prompt-for-symbol nil)

(evil-define-key 'normal clojure-mode-map (kbd "<C-S-return>") 'cider-eval-defun-at-point)
(evil-define-key 'insert clojure-mode-map (kbd "<C-S-return>") 'cider-eval-defun-at-point)

(evil-define-key 'normal clojure-mode-map (kbd "<C-return>") 'cider-eval-last-sexp)
(evil-define-key 'insert clojure-mode-map (kbd "<C-return>") 'cider-eval-last-sexp)
(evil-define-key 'visual clojure-mode-map (kbd "<C-return>") 'cider-eval-region)

;; Common Lisp
(package-require 'slime-company)

;; In my case /path/to/quicklisp is ~/quicklisp
(defvar quicklisp-path "~/quicklisp")
;;
;; Load slime-helper, this sets up various autoloads:
;;
(load (concat quicklisp-path "/slime-helper"))
;;
;; Set up slime with whatever modules you decide to use:
;;
(slime-setup '(slime-fancy slime-mrepl slime-banner slime-tramp
	       slime-xref-browser slime-highlight-edits
	       slime-sprof slime-company))
;;
;; Decide where to put a slime scratch file, if you want one, and set
;; it up with:
;;
(setf slime-scratch-file "/tmp/.slime-scratch.lisp")
;;
;; Unfortunately there is no hook for the slime-scratch mode so if you
;; want to automatically enable/disable various minor modes in the
;; slime scratch buffer you must do something like:
;;
;; (defadvice slime-scratch
;;     (after slime-scratch-adjust-modes () activate compile)
;;   (turn-some-mode-off)
;;   (turn-some-other-mode-on))

(add-to-list 'evil-emacs-state-modes 'slime-repl-mode)
(ruin/window-movement-for-mode "slime-repl" 'slime-repl-mode-map)
(evil-leader/set-key-for-mode 'lisp-mode
  "mc" 'slime-connect
  "md" 'slime-disconnect
  "eb" 'slime-eval-buffer
  "ee" 'slime-eval
  "es" 'slime-eval-last-expression
  "ed" 'slime-eval-defun

  "dd" 'slime-documentation)

(provide 'ruin-lisp)
