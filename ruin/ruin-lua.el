(package-require 'lua-mode)
(package-require 'highlight-numbers)
(package-require 'format-all)
(package-require 'realgud)
; (require 'doxymacs)
;(require 'doxymacs-luadoc)
;(load "~/build/work/realgud-mobdebug/realgud-mobdebug.el")
(require 'lua-mode)
(require 'lua-block)

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.luadoc$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.ld$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.luacheckrc$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.luacompleterc$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(add-hook 'lua-mode-hook 'highlight-numbers-mode)
(add-hook 'lua-mode-hook 'yas-minor-mode)
(add-hook 'lua-mode-hook 'flycheck-mode)
(add-hook 'lua-mode-hook 'doxymacs-mode)
(add-hook 'lua-mode-hook 'company-mode)
(add-hook 'lua-mode-hook 'lua-block-mode)
(add-hook 'lua-mode-hook 'eldoc-mode)
(add-hook 'lua-mode-hook 'smartparens-mode)
(add-hook 'lua-mode-hook (lambda ()
                           (setq compilation-auto-jump-to-first-error t)
                           (when (projectile-project-p)
                             (setq-local tags-file-name (string-join (list (projectile-project-root) "TAGS"))))
                           (setq-local eldoc-documentation-function 'ruin/etags-eldoc-function)
                           (define-key lua-mode-map (kbd "RET") 'indent-new-comment-line)
                           (if-let* ((cmd-buffer (get-buffer "*mobdebug main.lua shell*"))
                                     (proc (get-buffer-process cmd-buffer)))
                               (realgud:attach-cmd-buffer cmd-buffer))))
                                        ; (add-hook 'before-save-hook (lambda ()
                                        ;                               (when (equal major-mode 'lua-mode)
                                        ;                                 (format-all-buffer))))

(defun ruin/regenerate-ltags ()
  (interactive)
  (when (projectile-project-p)
    (let ((default-directory (projectile-project-root)))
      (shell-command "ltags -nr -e **/*.lua")
      (when (projectile-project-p)
        (setq-local tags-file-name (string-join (list (projectile-project-root) "TAGS"))))
      (message "TAGS regenerated."))))

(setq lua-default-application "luajit"
      tags-revert-without-query t
      tags-case-fold-search nil
      initial-buffer-choice "/home/ruin/build/elona-next/src/scratch.lua")

(defun ruin/get-lua-result ()
  "Gets the last line of the current Lua buffer."
  (with-current-buffer "*lua*"
    (sleep-for 0 200)
    (goto-char (point-max))
    (forward-line -1)
    (let ((line (thing-at-point 'line t)))
      (substring line 2 (- (length line) 1)))))

(defun ruin/run-lua (cmd)
  "Run CMD in the current Lua buffer and return the last line of the result."
  (lua-send-string (concat "return " cmd))
  (ruin/get-lua-result))

(defvar ruin/lua-initialized nil)

(defun ruin/initialize-lua ()
  "Load the data querying functions of the data table."
  (interactive)
  (with-temp-buffer
    (insert-file-contents "/home/ruin/build/script/lua/load_data.lua")
    (lua-send-buffer)))

(defun ruin/query-lua-data (arg type &optional id)
  "Query for entries of TYPE with ID and replace the symbol at point with it.

If ARG is set, don't replace the symbol."
  (interactive "P\nsType? ")
  (let* ((the-id (or id (thing-at-point 'symbol t)))
         (result (ruin/run-lua (concat "query_data(\"" type "\", " the-id ")"))))
    (if arg
        (message result)
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (delete-region (car bounds) (cdr bounds))
        (insert (concat "\"" result "\""))
        (goto-char (car bounds))))
    result))

(defun ruin/query-lua-data-chara (arg &optional id)
  "Query for core.chara with ID and replace the symbol at point with it.

If ARG is set, don't replace the symbol."
  (interactive "P")
  (ruin/query-lua-data arg "core.chara" id))

(defun ruin/query-lua-data-item (arg &optional id)
  "Query for core.item with ID and replace the symbol at point with it.

If ARG is set, don't replace the symbol."
  (interactive "P")
  (ruin/query-lua-data arg "core.item" id))

(defun ruin/edit-console-lua ()
  "Edit console.lua."
  (interactive)
  (find-file "/home/ruin/build/kuusou/copy/user/script/console.lua"))

(defun ruin/start-mobdebug ()
  (interactive)
  (mobdebug)
  (if-let ((cmd-buffer (get-buffer "*mobdebug main.lua shell*")))
      (realgud:attach-cmd-buffer cmd-buffer)))

(defun ruin/xref-find-definitions ()
  (interactive)
  (if-let ((sym (symbol-at-point)))
      (xref-find-definitions (symbol-name sym))
    (call-interactively 'xref-find-definitions)))

(defun ruin/xref-find-references ()
  (interactive)
  (if-let ((sym (symbol-at-point)))
      (xref-find-references (symbol-name sym))
    (call-interactively 'xref-find-references)))

(defun ruin/dotted-symbol-at-point ()
  (interactive)
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?. "_")
    (symbol-at-point)))

(defun ruin/xref-find-definitions-period ()
  (interactive)
  (xref-find-definitions (symbol-name (ruin/dotted-symbol-at-point))))

(defun ruin/xref-find-references-period ()
  (interactive)
  (xref-find-references (symbol-name (ruin/dotted-symbol-at-point))))

(progn
  (defun ruin/setup-lua-keybinds (mode)
    (evil-leader/set-key-for-mode mode
      "fg" 'ruin/xref-find-references
      "fG" 'ruin/xref-find-references-period
      "fd" 'elona-next-jump-to-definition
      "fj" 'ruin/xref-find-definitions
      "fJ" 'ruin/xref-find-definitions-period
      "mi" 'elona-next-start-repl
      "mr" 'elona-next-require-this-file
      "eb" 'elona-next-hotload-this-file
      "eB" 'elona-next-send-buffer
      "ee" 'elona-next-eval-expression
      "er" 'elona-next-require-this-file
      "el" 'elona-next-eval-current-line
      "ei" 'elona-next-insert-require
      "eI" 'elona-next-insert-missing-requires
      "ey" 'elona-next-copy-require-path
      "dd" 'elona-next-describe-thing-at-point
      "da" 'elona-next-describe-apropos
      "md" 'ruin/start-mobdebug
      "mt" 'ruin/regenerate-ltags
      "mf" 'kotaro-format-buffer))

  (mapc 'ruin/setup-lua-keybinds '(lua-mode fennel-mode)))

(define-key lua-mode-map (kbd "M-:") 'elona-next-eval-expression)

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("lua5.3" "/home/ruin/build/util/lua-lsp/bin/lua-lsp"))
                    :major-modes '(lua-mode)
                    :priority -1
                    :multi-root t
                    :server-id 'lua-lsp)))

(setq realgud-safe-mode nil)

(defun ruin/doxymacs--enter-insert (&rest _ignore)
  (evil-append 0))

(add-function :after (symbol-function 'doxymacs-call-template) #'ruin/doxymacs--enter-insert)

(setq tempo-interactive t)

(let ((file (if (eq system-type 'windows-nt)
                "z:/build/elona-next/src/elona-next.el"
              "/home/ruin/build/elona-next/src/elona-next.el")))
  (when (file-exists-p file)
    (load file)
    (elona-next-eval-sexp-fu-setup)))

(let ((file "/home/ruin/build/work/kotaro/kotaro.el"))
  (when (file-exists-p file)
    (load file)))

(require 'xref)
(defun xref--show-xref-buffer (fetcher alist)
  (cl-assert (functionp fetcher))
  (let* ((xrefs
          (or
           (assoc-default 'fetched-xrefs alist)
           (funcall fetcher)))
         (xref-alist (xref--analyze xrefs)))
    (with-current-buffer (get-buffer-create xref-buffer-name)
      (xref--xref-buffer-mode)
      (xref--show-common-initialize xref-alist fetcher alist)
      (display-buffer (current-buffer))
      (next-error)
      (current-buffer))))

(defun xref--show-defs-buffer-at-bottom (fetcher alist)
  "Show definitions list in a window at the bottom.
When there is more than one definition, split the selected window
and show the list in a small window at the bottom.  And use a
local keymap that binds `RET' to `xref-quit-and-goto-xref'."
  (let ((xrefs (funcall fetcher)))
    (cond
     ((not (cdr xrefs))
      (xref-pop-to-location (car xrefs)
                            (assoc-default 'display-action alist)))
     (t
      (with-current-buffer (get-buffer-create xref-buffer-name)
        (xref--transient-buffer-mode)
        (xref--show-common-initialize (xref--analyze xrefs) fetcher alist)
        (display-buffer (current-buffer)
                       '(display-buffer-in-direction . ((direction . below))))
        (next-error)
        (current-buffer))))))

(defun ruin/etags-eldoc-function ()
  (if (and elona-next--eldoc-saved-message
           (equal elona-next--eldoc-saved-point (point)))
      elona-next--eldoc-saved-message

    (setq elona-next--eldoc-saved-message nil
          elona-next--eldoc-saved-point nil)
    (elona-next-eldoc-function)
    (let* ((sym-dotted (ruin/dotted-symbol-at-point))
           (sym (symbol-at-point))
           (defs (or (and sym-dotted (etags--xref-find-definitions (prin1-to-string sym-dotted)))
                     (and sym (etags--xref-find-definitions (prin1-to-string sym))))))
      (when defs
        (let* ((def (car defs))
               (raw (substring-no-properties (xref-item-summary def))))
          (with-temp-buffer
            (insert raw)
            (delay-mode-hooks (lua-mode))
            (font-lock-default-function 'lua-mode)
            (font-lock-default-fontify-region (point-min)
                                              (point-max)
                                              nil)
            (buffer-string)))))))

(advice-add 'elona-next--command-jump-to :after
            (lambda (&rest args)
              (pulse-momentary-highlight-one-line (point))))

(add-to-list 'compilation-error-regexp-alist
             '("^[ \t]*\\([^ \t:\\[]+\\):\\([0-9]+\\):" 1 2))

(setq lua-font-lock-keywords
  `(;; highlight the hash-bang line "#!/foo/bar/lua" as comment
    ("^#!.*$" . font-lock-comment-face)

    ;; Builtin constants
    (,(lua-rx (symbol "true" "false" "nil"))
     . font-lock-constant-face)

    ;; Keywords
    (,(lua-rx lua-keyword)
     . font-lock-keyword-face)

    ;; Labels used by the "goto" statement
    ;; Highlights the following syntax:  ::label::
    (,(lua-rx "::" ws lua-name ws "::")
      . font-lock-constant-face)

    ;; Hightlights the name of the label in the "goto" statement like
    ;; "goto label"
    (,(lua-rx (symbol (seq "goto" ws+ (group-n 1 lua-name))))
      (1 font-lock-constant-face))

    ;; Highlight Lua builtin functions and variables
    (,lua--builtins
     (1 font-lock-builtin-face) (2 font-lock-builtin-face nil noerror))

    ("^[ \t]*\\_<for\\_>"
     (,(lua-make-delimited-matcher (lua-rx lua-name) ","
                                   (lua-rx (or (symbol "in") lua-assignment-op)))
      nil nil
      (1 font-lock-variable-name-face nil noerror)
      (2 font-lock-warning-face t noerror)
      (3 font-lock-warning-face t noerror)))

    ;; Handle local variable/function names
    ;;  local blalba, xyzzy =
    ;;        ^^^^^^  ^^^^^
    ;;
    ;;  local function foobar(x,y,z)
    ;;                 ^^^^^^
    ;;  local foobar = function(x,y,z)
    ;;        ^^^^^^
    ("^[ \t]*\\_<local\\_>"
     (0 font-lock-keyword-face)

     ;; (* nonl) at the end is to consume trailing characters or otherwise they
     ;; delimited matcher would attempt to parse them afterwards and wrongly
     ;; highlight parentheses as incorrect variable name characters.
     (,(lua-rx point ws lua-funcheader (* nonl))
      nil nil
      (1 font-lock-function-name-face nil noerror))

     (,(lua-make-delimited-matcher (lua-rx lua-name) ","
                                   (lua-rx lua-assignment-op))
      nil nil
      (1 font-lock-variable-name-face nil noerror)
      (2 font-lock-warning-face t noerror)
      (3 font-lock-warning-face t noerror)))

    (,(lua-rx (or bol ";") ws lua-funcheader)
     (1 font-lock-function-name-face))

    (,(lua-rx (or (group-n 1
                           "@" (symbol "author" "copyright" "field" "release"
                                       "return" "see" "usage" "description"))
                  (seq (group-n 1 "@" (symbol "param" "class" "name" "table" "field" "function")) ws+
                       (group-n 2 (+ (not (any "\n" space)))))))
     (1 font-lock-keyword-face t)
     (2 font-lock-variable-name-face t noerror))

    (,(lua-rx (seq (group-n 1 "@" (symbol "treturn")) (group-n 3 (or (seq "[" (+ alnum) "]") "")) ws+
                   (group-n 2 (+ (not (any "\n" space))))))
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t noerror)
     (3 font-lock-type-face t noerror))

    (,(lua-rx (seq (group-n 1 "@" (symbol "tparam")) (group-n 4 (or (seq "[" (+ alnum) "]") "")) ws+
                   (group-n 2 (+ (not (any "\n" space)))) ws+
                   (group-n 3 (+ (not (any "\n" space))))))
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t noerror)
     (3 font-lock-variable-name-face t noerror)
     (4 font-lock-type-face t noerror))))

(provide 'ruin-lua)
