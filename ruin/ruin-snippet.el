(package-require 'yasnippet)

(require 'yasnippet)

;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets"))

(yas-global-mode 1)

;; The following is optional.
;; (define-key yas-minor-mode-map [backtab]     'yas-expand)
;(evil-define-minor-mode-key 'insert 'yas-minor-mode [backtab] 'yas-expand)

;; Strangely, just redefining one of the variations below won't work.
;; All rebinds seem to be needed.
(define-key yas-minor-mode-map [(tab)]        'yas-expand)
(define-key yas-minor-mode-map (kbd "TAB")    'yas-expand)
(define-key yas-minor-mode-map (kbd "<tab>")  'yas-expand)

(add-hook 'kotlin-mode-hook 'yas-minor-mode-on)

(add-hook 'term-mode-hook
	  (lambda() (setq yas-dont-activate t)))

(add-hook 'magit-mode-hook
	  (lambda() (setq yas-dont-activate t)))

(defun ruin/aya-expand ()
  (interactive)
  (aya-expand)
  (evil-insert 0))

(package-require 'auto-yasnippet)
(global-set-key (kbd "M-y") #'aya-create)
(global-set-key (kbd "M-w") #'ruin/aya-expand)

(provide 'ruin-snippet)
