;;; ruin-htnk.el --- settings for HTML/ERB
(package-require 'web-mode)
(package-require 'haml-mode)
(package-require 'rainbow-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(add-hook 'web-mode-hook
      (lambda ()
        ;; short circuit js mode and just do everything in jsx-mode
        (if (equal web-mode-content-type "javascript")
            (web-mode-set-content-type "jsx")
          (message "now set to: %s" web-mode-content-type))))

(setq web-mode-markup-indent-offset 2)

(dolist (hook
         '(css-mode-hook web-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-turn-on))

(add-hook 'haml-mode-hook
  (function (lambda ()
          (setq evil-shift-width haml-indent-offset))))

(package-require 'scss-mode)

;; (evil-define-key 'insert web-mode-map (kbd "C-e") 'web-mode-element-close)

(provide 'ruin-html)
