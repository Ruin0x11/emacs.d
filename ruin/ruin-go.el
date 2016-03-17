;;; ruin.go.el --- settings for the Go language

(package-require 'go-mode)

(evil-leader/set-key-for-mode 'go-mode
  "mgf" 'go-goto-function
  "mgf" 'go-goto-arguments
  "mgd" 'go-goto-docstring
  "mgi" 'go-goto-imports
  "mgr" 'go-goto-return-values
  "mgm" 'go-goto-method-receiver
  "mia" 'go-import-add
  "mir" 'go-remove-unused-imports
  "mD" 'godoc
  "md" 'godoc-at-point
  )

(ruin/window-movement-for-mode "go-mode" 'godoc-mode-map)

(provide 'ruin-go)
