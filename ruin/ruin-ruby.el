;;; ruin-ruby.el --- settings for Ruby
(package-require 'robe)
(package-require 'enh-ruby-mode)
;(package-require 'ruby-block)
(package-require 'rspec-mode)
(package-require 'chruby)
(package-require 'yari) ;ルビヌスの槍
(package-require 'bundler)
(package-require 'minitest)
;(require 'flay)
;; (require 'rcodetools)

(require 'chruby)
(when (chruby-rubies)
  (chruby (car (chruby-rubies))))

(setq ruby-align-to-stmt-keywords '(def case)  ;; indent "case" as per ruby style guide
      ruby-indent-tabs-mode t
      enh-ruby-indent-tabs-mode t)

;(require 'ruby-block)
;(ruby-block-mode t)
;(setq ruby-block-highlight-toggle 'overlay    ;; do overlay
;      ruby-block-highlight-toggle 'minibuffer ;; display to minibuffer
;      ruby-block-highlight-toggle t)          ;; display to minibuffer and do overlay

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'highlight-numbers-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'smartparens-mode)
(add-hook 'enh-ruby-mode-hook 'minitest-mode)
(add-hook 'enh-ruby-mode-hook 'highlight-numbers-mode)
(add-hook 'ruby-mode-hook '(lambda ()
                                (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'ruby-mode-hook 'smartparens-mode)

(add-to-list 'auto-mode-alist '("Gemfile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(setq ruby-align-chained-calls t

      enh-ruby-program "ruby"
      ;enh-ruby-comment-column 32
      ;enh-ruby-bounce-deep-indent t
      ;enh-ruby-deep-indent-paren t
      ;enh-ruby-hanging-brace-deep-indent-level 1
      ;enh-ruby-hanging-brace-indent-level 2
      ;enh-ruby-hanging-indent-level 2
      ;enh-ruby-hanging-paren-deep-indent-level 0
      ;enh-ruby-hanging-paren-indent-level 2
      ;enh-ruby-indent-level 2

      inf-ruby-default-implementation "pry")

(ruin/set-shift-width-for-mode 'enh-ruby-mode-hook 'enh-ruby-indent-level)

(setq rake-completion-system 'helm)

(evil-define-key 'insert enh-ruby-mode-map (kbd "RET") 'evil-ret-and-indent)

(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)

(add-hook 'enh-ruby-mode-hook
          #'(lambda ()
              (yas-activate-extra-mode 'ruby-mode)))

(add-hook 'ruby-mode-hook
          (function (lambda ()
                      (setq evil-shift-width enh-ruby-indent-level))))

(add-hook 'rspec-mode-hook
          #'(lambda ()
              (setq compilation-scroll-output nil)))

(eval-after-load 'company
  '(push 'company-robe company-backends))

(defun ruby-insert-end ()
  (interactive)
  (if (eq (char-syntax (char-before)) ?w)
      (insert " "))
  (insert "end")
  (save-excursion
    (if (eq (char-syntax (char-after)) ?w)
        (insert " "))
    (ruby-indent-line t)
    (end-of-line)))

(defun ruby-brace-to-do-end ()
  (when (looking-at "{")
    (let ((orig (point)) (end (progn (ruby-forward-sexp) (point))))
      (when (eq (char-before) ?\})
        (delete-char -1)
        (if (eq (char-syntax (char-before)) ?w)
            (insert " "))
        (insert "end")
        (if (eq (char-syntax (char-after)) ?w)
            (insert " "))
        (goto-char orig)
        (delete-char 1)
        (if (eq (char-syntax (char-before)) ?w)
            (insert " "))
        (insert "do")
        (when (looking-at "\\sw\\||")
          (insert " ")
          (backward-char))
        t))))

(defun ruby-do-end-to-brace ()
  (when (and (or (bolp)
                 (not (memq (char-syntax (char-before)) '(?w ?_))))
             (looking-at "\\<do\\(\\s \\|$\\)"))
    (let ((orig (point)) (end (progn (ruby-forward-sexp) (point))))
      (backward-char 3)
      (when (looking-at ruby-block-end-re)
        (delete-char 3)
        (insert "}")
        (goto-char orig)
        (delete-char 2)
        (insert "{")
        (if (looking-at "\\s +|")
            (delete-char (- (match-end 0) (match-beginning 0) 1)))
        t))))

(defun ruby-toggle-block ()
  (interactive)
  (or (ruby-brace-to-do-end)
      (ruby-do-end-to-brace)))

(defun yari-helm-rehash ()
  (interactive)
  (setq yari-ruby-obarray-cache nil)
  (yari-helm))

(defvar yari-helm-source-ri-pages
  '((name . "RI documentation")
    (candidates . (lambda () (yari-ruby-obarray)))
    (action  ("Show with Yari" . yari))
    (candidate-number-limit . 300)
    (requires-pattern . 2)
    "Source for completing RI documentation."))

(defun yari-helm (&optional rehash)
  (interactive (list current-prefix-arg))
  (when current-prefix-arg (yari-ruby-obarray rehash))
  (helm :sources 'yari-helm-source-ri-pages :buffer "*yari*"))

(evil-define-key 'normal yari-mode-map "q" 'quit-window)

(evil-leader/set-key-for-mode 'enh-ruby-mode
  "mi" 'inf-ruby
  "dd" 'robe-doc
  "mm" 'robe-jump-to-module
  "fs" 'robe-jump
  "my" 'yari-helm
  "mo" 'robe-start
  "me" 'robe-rails-refresh

  "mbi" 'bundle-install
  "mbe" 'bundle-exec
  "mbs" 'bundle-show
  "mbu" 'bundle-update

  "ed" 'ruby-send-definition
  "eD" 'ruby-send-definition-and-go
  "eb" 'ruby-send-buffer
  "eB" 'ruby-send-buffer-and-go
  "es" 'ruby-send-last-sexp
  "eS" 'ruby-send-last-sexp-and-go
  "el" 'ruby-send-line
  "eL" 'ruby-send-line-and-go

  "mY" 'yari-helm-rehash
  ;"tt" 'rspec-verify
  ;ta" 'rspec-verify-all
  "tt" 'minitest-verify
  "ta" 'minitest-verify-all
  "tr" 'minitest-rerun
  "ts" 'minitest-verify-single
  ;"tr" 'rspec-run-last-failed
  "tj" 'rspec-find-spec-or-target-other-window
  "th" 'helm-feature-snippets

  "mg" 'helm-rubygems-org)

(evil-leader/set-key-for-mode 'haml-mode
  "my" 'yari-helm
  "mY" 'yari-helm-rehash)

(evil-leader/set-key-for-mode 'haml-mode
  "dd" 'robe-doc)

(evil-leader/set-key-for-mode 'web-mode
  "dd" 'robe-doc)

(advice-add 'minitest--run-command :before
            (lambda (&rest args)
              (save-some-buffers (not compilation-ask-about-save)
                                 compilation-save-buffers-predicate)))

(add-to-list 'evil-emacs-state-modes 'inf-ruby-mode)
(ruin/window-movement-for-mode "inf-ruby" 'inf-ruby-mode-map)

;;;###autoload
(defun ruby-indent-enable-on-save ()
  "Indent when saving buffer."
  (interactive)
  (add-hook 'before-save-hook #'indent-buffer nil t))

;; ;; autostart inf-ruby and robe
;; (dolist (hook (list
;;                'enh-ruby-mode-hook
;;                ))
;;   (add-hook hook (lambda ()
;;                    (robe-mode)
;;                    (save-excursion
;;                      (window-configuration-to-register 'a)
;;                      (inf-ruby)
;;                      (robe-start)
;;                      (jump-to-register 'a)
;;                      )
;;                    )))

;;; Rails
(package-require 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
;(add-hook 'projectile-mode-hook 'minitest-mode)

(evil-leader/set-key
  "mrm" 'projectile-rails-find-model
  "mrM" 'projectile-rails-find-current-model
  "mrI" 'projectile-rails-find-current-migration
  "mri" 'projectile-rails-find-migration
  "mrc" 'projectile-rails-find-controller
  "mrC" 'projectile-rails-find-current-controller
  "mrv" 'projectile-rails-find-view
  "mrV" 'projectile-rails-find-current-view
  "mrk" 'projectile-rails-find-rake-task
  "mra" 'projectile-rails-find-stylesheet
  "mrs" 'projectile-rails-server
  "mrS" 'projectile-rails-goto-seeds
  "mrA" 'projectile-rails-goto-schema
  "mro" 'projectile-rails-console
  "mrb" 'projectile-rails-dbconsole
  "mrf" 'projectile-rails-find-feature
  "mrj" 'projectile-rails-find-javascript
  "mry" 'projectile-rails-find-stylesheet
  "mrG" 'projectile-rails-goto-gemfile
  "mrg" 'projectile-rails-generate
  "mrr" 'projectile-rails-rake
  "mrR" 'projectile-rails-goto-routes
  "mrt" 'projectile-rails-find-test)

;; (add-hook 'projectile-rails-mode-hook
;;           '(lambda ()
;;              (evil-leader/set-local-key
;;                "mrm" 'projectile-rails-find-model
;;                "mrM" 'projectile-rails-find-current-model
;;                "mrc" 'projectile-rails-find-controller
;;                "mrC" 'projectile-rails-find-current-controller
;;                "mrv" 'projectile-rails-find-view
;;                "mrV" 'projectile-rails-find-current-view
;;                "mrk" 'projectile-rails-find-rake-task
;;                "mrs" 'projectile-rails-server
;;                "mrr" 'projectile-rails-console
;;                "mrg" 'projectile-rails-goto-gemfile
;;                "mrR" 'projectile-rails-goto-routes
;;                )
;;              ))

;; Setup compilation errors for minitest
;;(add-to-list 'compilation-error-regexp-alist 'minitest)
;;(add-to-list 'compilation-error-regexp-alist-alist '(minitest
;;                                                     ;;"^[\t ]*bin/rails test \\(.*\\):\\([1-9][0-9]*\\)"
;;                                                     "^[\t ]*\\(.*\\) \\[\\(.*\\):\\([1-9][0-9]*\\)\\]:"
;;                                                     2 3))

(add-to-list 'compilation-error-regexp-alist
             '(": from \\([^ \t:\\[]+\\):\\([0-9]+\\):in" 1 2))

(provide 'ruin-ruby)
