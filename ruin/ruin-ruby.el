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

(setq enh-ruby-program "ruby")

(setq enh-ruby-deep-indent-paren nil)
(setq enh-ruby-comment-column 32 )
(setq enh-ruby-bounce-deep-indent t )
(setq enh-ruby-deep-indent-paren t )
(setq enh-ruby-hanging-brace-deep-indent-level 1 )
(setq enh-ruby-hanging-brace-indent-level 2 )
(setq enh-ruby-hanging-indent-level 2 )
(setq enh-ruby-hanging-paren-deep-indent-level 0 )
(setq enh-ruby-hanging-paren-indent-level 2 )
(setq enh-ruby-indent-level 2 )

(evil-define-key 'insert enh-ruby-mode-map (kbd "RET") 'evil-ret-and-indent)

(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)

(add-hook 'enh-ruby-mode-hook 'robe-mode)

(add-hook 'ruby-mode-hook
          (function (lambda ()
                      (setq evil-shift-width enh-ruby-indent-level))))

(eval-after-load 'company
  '(push 'company-robe company-backends))

(evil-leader/set-key-for-mode 'enh-ruby-mode
  "mi" 'inf-ruby
  "md" 'robe-doc)

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

(provide 'ruin-ruby)
