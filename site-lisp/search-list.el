(require 'tabulated-list)

(defvar search-list-hook nil)

(defvar search-list-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for search-list major mode")

(defun search-list-go ()
  (select-window (split-window-vertically (selected-window) 20))
  )

(defun search-list-mode ()
  "Major mode for retrieving search results in a list"
  (interactive)
  (kill-all-local-variables)
  (use-local-map search-list-map)

(provide 'search-list)
