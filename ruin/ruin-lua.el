(package-require 'lua-mode)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(add-to-list 'compilation-error-regexp-alist-alist
         '(love "^\\(Error: \\|luajit: \\)?[ \t]*\\(Syntax error: \\)?\\([\\.\\\
/a-zA-Z0-9_]+\\.lua\\):\\([0-9]+\\):" 3 4))
(add-to-list 'compilation-error-regexp-alist 'love)

(provide 'ruin-lua)
