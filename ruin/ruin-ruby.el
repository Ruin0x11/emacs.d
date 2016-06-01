;;; ruin-ruby.el --- settings for Ruby
(package-require 'robe)
(package-require 'enh-ruby-mode)
(package-require 'ruby-block)
(package-require 'rspec-mode)
(package-require 'chruby)
(package-require 'yari) ;ルビヌスの槍
;; (require 'rcodetools)

(chruby "ruby-2.3.1")

(setq ruby-align-to-stmt-keywords '(def case)) ;; indent "case" as per ruby style guide

(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle 'overlay    ;; do overlay
      ruby-block-highlight-toggle 'minibuffer ;; display to minibuffer
      ruby-block-highlight-toggle t)          ;; display to minibuffer and do overlay

(add-hook 'ruby-mode-hook 'robe-mode)

(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(setq enh-ruby-program "ruby"
      enh-ruby-deep-indent-paren nil
      enh-ruby-comment-column 32 
      enh-ruby-bounce-deep-indent t 
      enh-ruby-deep-indent-paren t 
      enh-ruby-hanging-brace-deep-indent-level 1 
      enh-ruby-hanging-brace-indent-level 2 
      enh-ruby-hanging-indent-level 2 
      enh-ruby-hanging-paren-deep-indent-level 0 
      enh-ruby-hanging-paren-indent-level 2 
      enh-ruby-indent-level 2)

(evil-define-key 'insert enh-ruby-mode-map (kbd "RET") 'evil-ret-and-indent)

(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)

(add-hook 'enh-ruby-mode-hook 'robe-mode)

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

(evil-leader/set-key-for-mode 'enh-ruby-mode
  "mi" 'inf-ruby
  "md" 'robe-doc
  "mb" 'ruby-toggle-block
  "dd" 'yari-helm
  "my" 'yari-helm
  "mY" 'yari-helm-rehash
  "tt" 'rspec-verify
  "ta" 'rspec-verify-all
  "tr" 'rspec-run-last-failed
  "tj" 'rspec-find-spec-or-target-other-window
  "th" 'helm-feature-snippets)

(evil-leader/set-key-for-mode 'haml-mode
  "my" 'yari-helm
  "mY" 'yari-helm-rehash)

(add-to-list 'evil-emacs-state-modes 'inf-ruby-mode)
(ruin/window-movement-for-mode "inf-ruby" 'inf-ruby-mode-map)

;; autostart inf-ruby and robe
(dolist (hook (list
               'enh-ruby-mode-hook
               ))
  (add-hook hook (lambda ()
                   (robe-mode)
                   (save-excursion
                     (window-configuration-to-register 'a)
                     (inf-ruby)
                     (robe-start)
                     (jump-to-register 'a)
                     )
                   )))

;;; Rails
(package-require 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(evil-leader/set-key
 "mrm" 'projectile-rails-find-model
 "mrM" 'projectile-rails-find-current-model
 "mrc" 'projectile-rails-find-controller
 "mrC" 'projectile-rails-find-current-controller
 "mrv" 'projectile-rails-find-view
 "mrV" 'projectile-rails-find-current-view
 "mrk" 'projectile-rails-find-rake-task
 "mra" 'projectile-rails-find-stylesheet
 "mrs" 'projectile-rails-server
 "mrr" 'projectile-rails-console
 "mrg" 'projectile-rails-goto-gemfile
 "mrR" 'projectile-rails-goto-routes
 )

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

(provide 'ruin-ruby)
