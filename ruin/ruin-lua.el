(package-require 'lua-mode)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.luadoc$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(provide 'ruin-lua)
