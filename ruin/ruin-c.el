(require 'google-c-style)
; Add cmake listfile names to the mode list.
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

(add-hook 'c-mode-common-hook 'google-set-c-style)

(package-require 'csharp-mode)
(add-hook 'csharp-mode-hook 'electric-pair-mode)

(autoload 'cmake-mode "/usr/share/cmake-3.6/editors/emacs/cmake-mode.el" t)

(provide 'ruin-c)
