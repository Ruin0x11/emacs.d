;;; ruin-ruby.el --- settings for Ruby
(package-require 'robe)
(package-require 'enh-ruby-mode)
(package-require 'ruby-block)
(package-require 'rspec-mode)
(package-require 'chruby)

(chruby "ruby-2.2.4")

(require 'ruby-block)
(ruby-block-mode t)
;; do overlay
(setq ruby-block-highlight-toggle 'overlay)
;; display to minibuffer
(setq ruby-block-highlight-toggle 'minibuffer)
;; display to minibuffer and do overlay
(setq ruby-block-highlight-toggle t)

(add-hook 'ruby-mode-hook 'robe-mode)

(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
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

(eval-after-load 'company
  '(push 'company-robe company-backends))

(evil-leader/set-key-for-mode 'enh-ruby-mode
  "mi" 'inf-ruby
  "md" 'robe-doc)

;; autostart inf-ruby and robe
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

(evil-leader/set-key-for-mode 'projectile-mode
  "mrm" 'projectile-rails-find-model
  "mrM" 'projectile-rails-find-current-model
  "mrc" 'projectile-rails-find-controller
  "mrC" 'projectile-rails-find-current-controller
  "mrv" 'projectile-rails-find-view
  "mrV" 'projectile-rails-find-current-view
  "mrk" 'projectile-rails-find-rake-task
  "mrR" 'projectile-rails-server
  "mrr" 'projectile-rails-console
  "mrgg" 'projectile-rails-goto-gemfile
  "mrgr" 'projectile-rails-goto-routes
  )

(provide 'ruin-ruby)
