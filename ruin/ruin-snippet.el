(package-require 'yasnippet)

(require 'yasnippet)

;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets"))

(yas-global-mode 1)

;; The following is optional.
(define-key yas-minor-mode-map [backtab]     'yas-expand)
;(evil-define-minor-mode-key 'insert 'yas-minor-mode [backtab] 'yas-expand)

;; Strangely, just redefining one of the variations below won't work.
;; All rebinds seem to be needed.
(define-key yas-minor-mode-map [(tab)]        nil)
(define-key yas-minor-mode-map (kbd "TAB")    nil)
(define-key yas-minor-mode-map (kbd "<tab>")  nil)

(add-hook 'term-mode-hook
	  (lambda() (setq yas-dont-activate t)))

(provide 'ruin-snippet)
