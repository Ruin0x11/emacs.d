;;; lookup-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "lookup" "lookup.el" (22248 21271 275567 345000))
;;; Generated autoloads from lookup.el

(autoload 'lookup "lookup" "\
Start Lookup and display the list of your dictionaries.
If you have already started lookup, display the last status of buffers.

\(fn)" t nil)

(autoload 'lookup-list-history "lookup" "\


\(fn)" t nil)

(autoload 'lookup-list-modules "lookup" "\


\(fn)" t nil)

(autoload 'lookup-pattern "lookup" "\
Search for the PATTERN with MODULE.

\(fn PATTERN &optional MODULE)" t nil)

(autoload 'lookup-pattern-full-screen "lookup" "\
Search for the PATTERN in full screen.
See `lookup-pattern' for details.

\(fn PATTERN &optional MODULE)" t nil)

(autoload 'lookup-pattern-other-frame "lookup" "\
Search for the PATTERN in another frame.
See `lookup-pattern' for details.

\(fn PATTERN &optional MODULE)" t nil)

(autoload 'lookup-word "lookup" "\
Search for the word near the cursor.

\(fn WORD &optional MODULE)" t nil)

(autoload 'lookup-word-full-screen "lookup" "\
Search for the word near the cursor in full screen.
See `lookup-word' for details.

\(fn WORD &optional MODULE)" t nil)

(autoload 'lookup-word-other-frame "lookup" "\
Search for the word near the cursor in another frame.
See `lookup-word' for details.

\(fn WORD &optional MODULE)" t nil)

(autoload 'lookup-region "lookup" "\
Search for the region.

\(fn START END &optional MODULE)" t nil)

(autoload 'lookup-region-full-screen "lookup" "\
Search for the region in full screen.
See `lookup-region' for details.

\(fn START END &optional MODULE)" t nil)

(autoload 'lookup-region-other-frame "lookup" "\
Search for the region in another frame.
See `lookup-region' for details.

\(fn START END &optional MODULE)" t nil)

(autoload 'lookup-selection "lookup" "\
Search for the mouse's selection.

\(fn CLICK)" t nil)

(autoload 'lookup-selection-full-screen "lookup" "\
Search for the mouse's selection in full screen.
See `lookup-selection' for details.

\(fn CLICK)" t nil)

(autoload 'lookup-selection-other-frame "lookup" "\
Search for the mouse's selection in another frame.
See `lookup-selection' for details.

\(fn CLICK)" t nil)

(autoload 'lookup-secondary "lookup" "\
Search for the mouse's secondary selection.

\(fn CLICK)" t nil)

(autoload 'lookup-secondary-full-screen "lookup" "\
Search for the mouse's secondary selection in full screen.
See `lookup-secondary' for details.

\(fn CLICK)" t nil)

(autoload 'lookup-secondary-other-frame "lookup" "\
Search for the mouse's secondary selection in another frame.
See `lookup-secondary' for details.

\(fn CLICK)" t nil)

;;;***

;;;### (autoloads nil "lookup-content" "lookup-content.el" (22248
;;;;;;  21271 268900 796000))
;;; Generated autoloads from lookup-content.el

(autoload 'lookup-content-mode "lookup-content" "\
\\{lookup-content-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "lookup-select" "lookup-select.el" (22248 21271
;;;;;;  272234 71000))
;;; Generated autoloads from lookup-select.el

(autoload 'lookup-select-dictionaries "lookup-select" "\


\(fn MODULE)" t nil)

;;;***

;;;### (autoloads nil "stem-english" "stem-english.el" (22248 21271
;;;;;;  285567 170000))
;;; Generated autoloads from stem-english.el

(autoload 'stem-english "stem-english" "\


\(fn STR)" nil nil)

;;;***

(provide 'lookup-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lookup-autoloads.el ends here
