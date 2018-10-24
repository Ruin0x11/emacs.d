;;; ruin-htnk.el --- settings for HTML/ERB
(package-require 'web-mode)
(package-require 'haml-mode)
(package-require 'rainbow-mode)
(package-require 'coffee-mode)
(package-require 'less)
(package-require 'flymake-less)
(package-require 'js-doc)
(package-require 'prettier-js)
(package-require 'ssass-mode)
(package-require 'slim-mode)
(package-require 'tidy)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . ssass-mode))

(add-hook 'web-mode-hook
      (lambda ()
        ;; short circuit js mode and just do everything in jsx-mode
        (if (equal web-mode-content-type "javascript")
            (web-mode-set-content-type "jsx")
          (message "now set to: %s" web-mode-content-type))))

(dolist (hook
         '(css-mode-hook web-mode-hook sass-mode-hook less-css-mode-hook))
  (add-hook hook 'rainbow-mode)
  (add-hook hook 'company-mode-on))

(add-hook 'less-css-mode-hook 'electric-pair-mode)

(setq web-mode-markup-indent-offset 2)
(setq slim-indent-offset 2)

(add-hook 'haml-mode-hook
  (function (lambda ()
          (setq evil-shift-width haml-indent-offset))))

(add-hook 'slim-mode-hook
  (function (lambda ()
          (setq evil-shift-width slim-indent-offset))))

(package-require 'scss-mode)

(add-hook 'js2-mode-hook 'prettier-js-mode)

(evil-leader/set-key-for-mode 'web-mode
  "ed" 'skewer-eval-defun
  "md" 'js-doc-insert-file-doc
  "bi" 'tidy-buffer)

;; (evil-define-key 'insert web-mode-map (kbd "C-e") 'web-mode-element-close)

(setq coffee-tab-width 2)

(provide 'ruin-html)
