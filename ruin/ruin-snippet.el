(package-require 'yasnippet)

(require 'yasnippet)

;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets"))

(yas-global-mode 1)

(add-hook 'term-mode-hook
	  (lambda() (setq yas-dont-activate t)))

(provide 'ruin-snippet)
