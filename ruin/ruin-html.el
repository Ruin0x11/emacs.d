;;; ruin-htnk.el --- settings for HTML/ERB
(package-require 'web-mode)
(package-require 'haml-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(setq web-mode-markup-indent-offset 2)

(package-require 'scss-mode)

;; (evil-define-key 'insert web-mode-map (kbd "C-e") 'web-mode-element-close)

(provide 'ruin-html) 
