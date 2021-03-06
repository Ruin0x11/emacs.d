(package-require 'eval-in-repl)
(package-require 'smartparens)
(package-require 'evil-smartparens)
(package-require 'lispyville)
;(package-require 'slamhound)
(package-require 'package-lint)
(package-require 'flycheck-package)
(package-require 'fennel-mode)
(require 'smartparens-config)
(require 'cl)
(require 'janet-mode)
(require 'eval-sexp-fu)

(setq lisp-modes
      '(scheme-mode emacs-lisp-mode lisp-mode
                    clojure-mode common-lisp-mode lisp-interaction-mode
                    cider-repl-mode inferior-emacs-lisp-mode sly-mrepl-mode
                    janet-mode inferior-lisp-mode fennel-mode))

(defun add-lisp-hook (func)
  (add-hooks lisp-modes func))

(global-eldoc-mode)
(add-lisp-hook 'eldoc-mode)
(add-lisp-hook 'smartparens-mode)
(add-lisp-hook 'lispyville-mode)

(smartparens-global-mode)

; (with-eval-after-load 'elisp-mode
;   (defadvice elisp-get-fnsym-args-string (after add-docstring activate compile)
;     "Add a 1st line of docstring to ElDoc's function information."
;     (when ad-return-value
;       (when-let ((doc (elisp--docstring-first-line (documentation (ad-get-arg 0) t))))
;         (let* ((w (frame-width))
;                (color-doc (propertize doc 'face 'font-lock-doc-face)))
;           (when (and doc (not (string= doc "")))
;             (setq ad-return-value (concat ad-return-value "\n" color-doc))
;             (when (> (length doc) w)
;               (setq ad-return-value (substring ad-return-value 0 (1- w)))))))
;       ad-return-value)))

;;; emacs lisp

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "eb" 'ruin/write-and-eval-buffer
  "es" 'eval-last-sexp
  "ed" 'eval-defun
  )

(evil-leader/set-key-for-mode 'lisp-interaction-mode
  "eb" 'ruin/write-and-eval-buffer
  "es" 'eval-last-sexp
  "ed" 'eval-defun
  )

(defun ruin/lisp-eval-last-sexp ()
  (interactive)
  (save-excursion
    (forward-char)
    (lisp-eval-last-sexp)))

(evil-leader/set-key-for-mode 'janet-mode
  "ee" 'janet-eval-expression
  "eb" 'janet-eval-buffer
  "es" 'ruin/lisp-eval-last-sexp
  "ed" 'lisp-eval-defun
  "dd" 'janet-doc
  )

(evil-leader/set-key
  "lc" 'lispy-convolute
  "lC" 'lispy-convolute-left
  "lO" 'lispy-oneline
  "lm" 'lispy-alt-multiline
  "lM" 'lispy-oneline
  "lS" 'lispy-stringify
  "l/" 'lispy-splice
  "lr" 'lispy-raise-some
  "lR" 'lispy-raise
  "lx" 'hydra-lispy-x/body
  "lp" 'lispy-clone
  )

(defun endless/eval-overlay (value point)
  (cider--make-result-overlay (format "%S" value)
    :where point
    :duration 'command)
  ;; Preserve the return value.
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (endless/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (endless/eval-overlay r (point))))

(advice-add 'eval-defun :filter-return
            (lambda (r)
              (endless/eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))

(add-hook 'edebug-mode-hook 'evil-emacs-state)
(add-hook 'inferior-lisp-mode-hook 'evil-emacs-state)
(add-hook 'ielm-mode-hook 'company-mode)

;; (define-key smartparens-mode-map (kbd "C-s") 'sp-forward-slurp-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-s") 'sp-forward-barf-sexp)

(with-eval-after-load 'lispyville
  (lispyville-set-key-theme
   '(operators
     (escape insert)
     (commentary normal visual motion)
     (additional-motions normal visual motion)
     (additional-insert)
     ;(text-objects normal visual motion)
     (slurp/barf-cp normal visual motion)
     (additional visual)))
  (lispyville--define-key '(normal visual motion)
           "[" #'lispyville-previous-opening
           "]" #'lispyville-next-opening
           "-" #'lispy-up
           "+" #'lispy-down
           ; (kbd "M-h") #'lispy-move-left
           ; (kbd "M-l") #'lispy-down-slurp
           "{" #'evil-backward-paragraph
           "}" #'evil-forward-paragraph
           "(" #'lispyville-backward-up-list
           ")" #'lispy-flow)
  (define-key lispy-mode-map (kbd "<C-return>") nil)
  )

(defun ruin/lispy-map--enter-insert (&rest _ignore)
  (evil-insert 0))

(add-function :after (symbol-function 'lispy-map-make-input-overlay) #'ruin/lispy-map--enter-insert)


(eval-after-load "ielm" #'(lambda ()
                            (ruin/window-movement-for-map inferior-emacs-lisp-mode-map)
                            (define-key inferior-emacs-lisp-mode-map (kbd "<down>") 'comint-next-input)
                            (define-key inferior-emacs-lisp-mode-map (kbd "<up>") 'comint-previous-input)))
(add-to-list 'evil-emacs-state-modes 'inferior-emacs-lisp-mode)

(define-eval-sexp-fu-flash-command eval-last-sexp
  (eval-sexp-fu-flash (when (ignore-errors (elisp--preceding-sexp))
                        (with-esf-end-of-sexp
                          (save-excursion
                            (backward-sexp)
                            (sp-beginning-of-sexp)
                            (backward-char)
                            (bounds-of-thing-at-point 'sexp))))))

(define-eval-sexp-fu-flash-command eval-defun
  (eval-sexp-fu-flash (bounds-of-thing-at-point 'defun)))

;;; clojure
(package-require 'cider)
(package-require 'cider-eval-sexp-fu)
;;(package-require 'clj-refactor)
(require 'cider-eval-sexp-fu)
(require 'eval-in-repl-cider)

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

(eval-after-load "lispy"
  `(progn
     (lispy-set-key-theme '(lispy c-digits))
     (define-key lispy-mode-map (kbd "<C-return>") 'eir-eval-in-cider)))

(eval-after-load "cider" #'(lambda ()
                             (ruin/window-movement-for-map cider-repl-mode-map)
                             (ruin/window-movement-for-map cider-docview-mode-map)
                             (ruin/window-movement-for-map cider-stacktrace-mode-map)
                             (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)
                             (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
                             (define-key cider-repl-mode-map (kbd "M-n") 'cider-repl-next-input)
                             (define-key cider-repl-mode-map (kbd "M-p") 'cider-repl-previous-input)
                             (define-key cider-repl-mode-map (kbd "C-c C-k") 'cider-repl-clear-buffer)
                             (define-key clojure-mode-map (kbd "C-x C-e") 'ruin/cider-eval-last-sexp-in-repl)
                             (define-key clojure-mode-map (kbd "<C-return>") 'eir-eval-in-cider)))

(add-to-list 'evil-emacs-state-modes 'cider-repl-mode)
(add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)
(add-to-list 'evil-emacs-state-modes 'cider-docview-mode)
(add-to-list 'evil-emacs-state-modes 'cider-popup-buffer-mode)

(defun ruin/cider-eval-last-sexp-in-repl ()
  (interactive)
  (eir-send-to-cider (cider-last-sexp)))

(evil-leader/set-key-for-mode 'clojure-mode
  "mj" 'cider-jack-in
  "ma" 'cider-apropos-documentation
  "mr" 'cider-switch-to-repl-buffer
  "mb" 'connect-burgundy
                                        ;  "ms" 'slamhound
  "mq" 'cider-quit

  "eb" 'cider-eval-buffer
  ;; "ee" 'cider-read-and-eval
  "es" 'ruin/cider-eval-last-sexp-in-repl
  "ed" 'cider-eval-defun-at-point
  "eD" 'cider-pprint-eval-defun-at-point
  "en" 'cider-repl-set-ns

  "Fs" 'cider-find-var

  "tt" 'cider-test-run-test
  "tn" 'cider-test-run-ns-tests
  "ta" 'cider-test-run-project-tests
  "td" 'cider-test-ediff
  "tb" 'cider-test-show-report
  "tr" 'cider-test-rerun-tests

  "dd" 'cider-doc)

(evil-leader/set-key-for-mode 'clojurescript-mode
  "mj" 'cider-jack-in-clojurescript
  "ma" 'cider-apropos-documentation
  "mr" 'cider-switch-to-repl-buffer
  "mc" 'cider-connect
  "ms" 'cider-create-sibling-cljs-repl
  "mq" 'cider-quit
  "mR" 'cider-completion-flush-caches

  "eb" 'cider-eval-buffer
  ;; "ee" 'cider-read-and-eval
  "es" 'ruin/cider-eval-last-sexp-in-repl
  "ed" 'cider-eval-defun-at-point
  "eD" 'cider-pprint-eval-defun-at-point
  "en" 'cider-repl-set-ns

  "Fs" 'cider-find-var

  "tt" 'cider-test-run-test
  "tn" 'cider-test-run-ns-tests
  "ta" 'cider-test-run-project-tests
  "td" 'cider-test-ediff
  "tb" 'cider-test-show-report
  "tr" 'cider-test-rerun-tests

  "dd" 'cider-doc)

(defun connect-burgundy ()
  (interactive)
  (start-file-process-shell-command "burgundy"
                                    (get-buffer-create "*burgundy*")
                                    "lein run")
  (sit-for 5)
  (cider-connect "localhost" 7777))

(setq cider-show-error-buffer 'except-in-repl
      cider-auto-select-error-buffer nil
      cider-doc-auto-select-buffer nil
      cider-prompt-for-symbol nil
      cider-repl-pop-to-buffer-on-connect nil
      cider-repl-use-clojure-font-lock t
      cider-repl-display-help-banner nil

      nrepl-prompt-to-kill-server-buffer-on-quit nil)

(add-to-list 'evil-emacs-state-modes 'cider-repl-mode)

(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

;; (evil-define-key 'normal clojure-mode-map (kbd "<C-S-return>") 'cider-eval-defun-at-point)
;; (evil-define-key 'insert clojure-mode-map (kbd "<C-S-return>") 'cider-eval-defun-at-point)

;; (evil-define-key 'normal clojure-mode-map (kbd "<C-return>") 'cider-eval-last-sexp)
;; (evil-define-key 'insert clojure-mode-map (kbd "<C-return>") 'cider-eval-last-sexp)
;; (evil-define-key 'visual clojure-mode-map (kbd "<C-return>") 'cider-eval-region)

(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-repl-mode-hook 'cider-repl-toggle-pretty-printing)


(autoload 'cider--make-result-overlay "cider-overlays")

;;; clisp
(comment
 (package-require 'sly)
 (package-require 'sly-quicklisp)
 (require 'sly-quicklisp)

 (setq inferior-lisp-program "sbcl --control-stack-size 100000")

 (add-to-list 'evil-emacs-state-modes 'sly-mrepl-mode)
 (add-to-list 'evil-emacs-state-modes 'sly-db-mode)
 (add-to-list 'evil-emacs-state-modes 'sly-apropos-mode)
 (add-to-list 'evil-emacs-state-modes 'sly-xref-mode)
 (add-to-list 'evil-emacs-state-modes 'sly-stickers--replay-mode)

 (add-hook 'sly-mrepl-mode-hook 'company-mode)
 (add-hook 'sly-mrepl-mode-hook (lambda () (yas-minor-mode 0)))

 (defun ruin/sly-last-expression ()
   (buffer-substring-no-properties
    (save-excursion
      (forward-char 1)
      (backward-sexp) (point))
    (+ 1 (point))))

 (defun ruin/sly-eval-last-expression ()
   "Evaluate the expression preceding point."
   (interactive)
   (sly-interactive-eval (ruin/sly-last-expression)))

 (defun ruin/sly-describe ()
   (interactive)
   (let ((current-prefix-arg '-))
     (call-interactively 'sly-describe-symbol)))

 (dolist (mode '(lisp-mode sly-mrepl-mode))
   (evil-leader/set-key-for-mode mode
     "dd" 'sly-describe-symbol
     "df" 'ruin/sly-describe
     "da" 'sly-apropos
     "dh" 'sly-hyperspec-lookup
     "ee" 'sly-interactive-eval
     "es" 'ruin/sly-eval-last-expression
     ;; "ed" 'sly-eval-defun
     "ed" 'sly-compile-defun
     "eb" 'sly-compile-and-load-file
     "ekk"  'sly-stickers-dwim
     "fd" 'sly-edit-definition
     "fD" 'sly-edit-definition-other-window

     "my" 'sly-mrepl-sync))

 (define-key sly-mode-map (kbd "M-.") 'sly-edit-definition)
 (define-key sly-mode-map (kbd "M-?") 'sly-edit-uses)
 (define-key sly-mode-map (kbd "C-t") 'sly-pop-find-definition-stack)
 (define-key evil-normal-state-map (kbd "M-.") nil)
 (define-key evil-insert-state-map (kbd "DEL") 'backward-delete-char-untabify)

 (add-hook 'sly-mrepl-mode-hook (lambda ()
                                  (define-key sly-mrepl-mode-map "\C-c\M-o" 'comint-clear-buffer)))

 (sp-with-modes '(sly-mrepl-mode)
   (sp-local-pair "'" nil :actions nil)))

;; (defun dood (r)
;;   (endless/eval-overlay r (point)))
;;
;; (advice-add 'sly-display-eval-result :after 'dood)

;; (advice-add 'sly-show-description :after
;;             (lambda (s p)
;;               (evil-emacs-state)))

;; (require 'info-look)
;; (info-lookup-add-help
;;  :mode 'lisp-mode
;;  :regexp "[^][()'\" \t\n]+"
;;  :ignore-case t
;;  :doc-spec '(("(gcl)Symbols Dictionary" nil nil nil)))

(provide 'ruin-lisp)
