; Add cmake listfile names to the mode list.
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

(autoload 'cmake-mode "/usr/share/cmake-3.6/editors/emacs/cmake-mode.el" t)

(package-require 'csharp-mode)

(provide 'ruin-c)
