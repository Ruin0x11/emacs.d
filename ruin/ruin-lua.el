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
;(add-hook 'lua-mode-hook 'doxymacs-mode)
(add-hook 'lua-mode-hook 'company-mode)
(add-hook 'lua-mode-hook 'lua-block-mode)
(add-hook 'lua-mode-hook 'eldoc-mode)
(add-hook 'lua-mode-hook 'smartparens-mode)
(add-hook 'lua-mode-hook (lambda ()
                           (setq compilation-auto-jump-to-first-error t)
                           (make-variable-buffer-local 'after-save-hook)
                           ;(add-to-list 'after-save-hook (lambda ()
                           ;                                (when (projectile-project-p)
                           ;                                  (let ((default-directory (projectile-project-root)))
                           ;                                    (shell-command (format "bash -e ltags -nr -e %s **/*.lua"))))))
                           (when (projectile-project-p)
                             (setq-local tags-file-name (string-join (list (projectile-project-root) "TAGS"))))
                           (setq-local eldoc-documentation-function 'ruin/etags-eldoc-function)
                           (setq compilation-error-regexp-alist (list (list lua-traceback-line-re 1 2)))
                           (define-key lua-mode-map (kbd "RET") 'indent-new-comment-line)

                           ;; luacheck for whatever reason cannot
                           ;; handle forward slashes in its --config
                           ;; option.
                           (setq flycheck-locate-config-file-functions
                                 '(ruin/flycheck-locate-config-file-ancestor-directories))

                           (if-let* ((cmd-buffer (get-buffer "*mobdebug main.lua shell*"))
                                     (proc (get-buffer-process cmd-buffer)))
                               (realgud:attach-cmd-buffer cmd-buffer))))
                                        ; (add-hook 'before-save-hook (lambda ()
                                        ;                               (when (equal major-mode 'lua-mode)
                                        ;                                 (format-all-buffer))))

(when (eq system-type 'windows-nt)
  (setq initial-buffer-choice '("z:/build/elona-next/src/scratch.lua")))

(defun ruin/flycheck-locate-config-file-ancestor-directories (file _checker)
  (when-let ((path (flycheck-locate-config-file-ancestor-directories file _checker)))
    (subst-char-in-string ?/ ?\\ path)))

(setq tags-revert-without-query t
      tags-case-fold-search nil)

(defun ruin/get-lua-result ()
  "Gets the last line of the current Lua buffer."
  (with-current-buffer "*lua*"
    (sleep-for 0 200)
    (goto-char (point-max))
    (forward-line -1)
    (let ((line (thing-at-point 'line t)))
      (substring line 2 (- (length line) 1))))
  )

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
  (xref-find-definitions (symbol-name (symbol-at-point))))

(defun ruin/xref-find-references ()
  (interactive)
  (xref-find-references (symbol-name (symbol-at-point))))

(defun ruin/xref-find-definitions-period ()
  (interactive)
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?. "_")
    (xref-find-definitions (symbol-name (symbol-at-point)))))

(defun ruin/xref-find-references-period ()
  (interactive)
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?. "_")
    (xref-find-references (symbol-name (symbol-at-point)))))

(evil-leader/set-key-for-mode 'lua-mode
  ; "mi" 'ruin/initialize-lua
  ; "mc" 'ruin/query-lua-data-chara
  ; "mt" 'ruin/query-lua-data-item
  ; "mq" 'ruin/query-lua-data
  ; "mo" 'ruin/edit-console-lua
  "fd" 'ruin/xref-find-definitions
  "fD" 'ruin/xref-find-definitions-period
  "fg" 'ruin/xref-find-references
  "fG" 'ruin/xref-find-references-period
  "mi" 'elona-next-start-repl
  "mr" 'elona-next-require-this-file
  "eb" 'elona-next-hotload-this-file
  "eB" 'elona-next-send-buffer
  "el" 'elona-next-send-current-line
  "er" 'elona-next-require-this-file
  "ey" 'elona-next-copy-require-path
  "ei" 'elona-next-insert-require
  "md" 'ruin/start-mobdebug
  ;"ee" 'realgud:cmd-eval
  ;"er" 'realgud:cmd-eval-region
  )

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
  (let* ((sym (prin1-to-string (symbol-at-point)))
         (defs (etags--xref-find-definitions sym)))
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
          (buffer-string))))))

;(add-to-list 'compilation-error-regexp-alist
;             '("\\(.+\\):\\([1-9][0-9]+\\) in " 1 2))
(add-to-list 'compilation-error-regexp-alist
             '(" in function <\\(.+\\):\\([1-9][0-9]+\\)>" 1 2))
(setq compilation-error-regexp-alist (list (list lua-traceback-line-re 1 2)))
;
; ;; nunit-console.exe on windows uses this format
; (add-to-list 'compilation-error-regexp-alist
;              '(" in \\(.+\\):line \\([0-9]+\\)" 1 2))
;
; ;; dotnet test with xunit project
; ;; [xUnit.net 00:00:00.6080370]         /TestProject/UnitTest1.cs(15,0): at TestProject.UnitTest1.Test1()
; (add-to-list 'compilation-error-regexp-alist '("\\[xUnit.net .*\\] +\\(.*\\)(\\([[:digit:]]+\\),\\([[:digit:]]+\\))" 1 2 3))

(package-require 'rainbow-mode)
(require 'rainbow-mode)
(setq rainbow-html-colors t)
;; (setq rainbow-html-colors-alist nil)

(add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
             '("{\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*[0-9]*\.?[0-9]+\s*%?\s*}"
               (0 (rainbow-colorize-rgb))))
(add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
             '("color(\s*\\([0-9]\\{1,3\\}\\(?:\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*)"
                (0 (rainbow-colorize-rgb))))
(add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
             '("{\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\)}"
               (0 (rainbow-colorize-rgb))))
(add-hook 'c++-mode-hook 'rainbow-mode)
(add-hook 'lua-mode-hook 'rainbow-mode)

(provide 'ruin-lua)
