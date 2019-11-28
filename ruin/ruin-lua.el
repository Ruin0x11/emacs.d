(package-require 'lua-mode)
(package-require 'highlight-numbers)
(package-require 'format-all)
(package-require 'realgud)
;(require 'doxymacs)
;(require 'doxymacs-luadoc)
;(load "~/build/work/realgud-mobdebug/realgud-mobdebug.el")
(load "~/build/elona-next.el")
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
(add-hook 'lua-mode-hook (lambda ()
                           (setq compilation-auto-jump-to-first-error t)
                           (doxymacs-mode t)
                           (company-mode t)
                           (lua-block-mode t)
                           (elona-next-minor-mode t)
                           (flycheck-mode t)
                           (define-key lua-mode-map (kbd "RET") 'indent-new-comment-line)
                           (if-let* ((cmd-buffer (get-buffer "*mobdebug main.lua shell*"))
                                     (proc (get-buffer-process cmd-buffer)))
                               (realgud:attach-cmd-buffer cmd-buffer))))
; (add-hook 'before-save-hook (lambda ()
;                               (when (equal major-mode 'lua-mode)
;                                 (format-all-buffer))))

(defun ruin/get-lua-result ()
  "Gets the last line of the current Lua buffer."
  (with-current-buffer "*lua*"
    (sleep-for 0 200)
    (goto-char (point-max))
    (forward-line -1)
    (let ((line (thing-at-point 'line t)))
      (substring line 2 (- (length line) 1)))))

(defun ruin/send-lua-string (cmd)
  "Run CMD in the current Lua buffer and return the last line of the result."
  (lua-send-string (concat "return " cmd))
  (ruin/get-lua-result))

(defvar ruin/run-lua-options-history '())

(defun ruin/run-lua (args)
  (interactive (list
                (split-string
                 (read-string
                  (format "Run: %s " lua-default-application)
                  (or (first ruin/run-lua-options-history) "")
                  'ruin/run-lua-options-history))))
  (let ((default-directory (or (projectile-project-root) default-directory))
        (lua-default-command-switches args))
    (unless (and (buffer-live-p lua-process-buffer)
                 (process-live-p (get-buffer-process lua-process-buffer)))
      (run-lua))
    (if (buffer-live-p lua-process-buffer)
      (popwin:display-buffer lua-process-buffer))))

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

(evil-leader/set-key-for-mode 'lua-mode
  ; "mi" 'ruin/initialize-lua
  ; "mc" 'ruin/query-lua-data-chara
  ; "mt" 'ruin/query-lua-data-item
  ; "mq" 'ruin/query-lua-data
  ; "mo" 'ruin/edit-console-lua
  "mi" 'ruin/run-lua
  "md" 'ruin/start-mobdebug
  ;"ee" 'realgud:cmd-eval
  ;"er" 'realgud:cmd-eval-region
  "ed" 'elona-next-send-defun
  "el" 'elona-next-send-buffer
  "eb" 'elona-next-hotload-this-file)

(add-to-list 'evil-emacs-state-modes 'comint-mode)

(require 'eval-sexp-fu)
(elona-next-eval-sexp-fu-setup)

(setq lua-default-application "luajit")

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("lua5.3" "/home/ruin/build/util/lua-lsp/bin/lua-lsp"))
                    :major-modes '(lua-mode)
                    :priority -1
                    :multi-root t
                    :server-id 'lua-lsp)))

(defun ruin/lua-privatize ()
  (interactive)
  (let ((stab (copy-syntax-table)))
    (with-syntax-table stab
      (modify-syntax-entry ?. "_")
      (modify-syntax-entry ?- ".")
      (modify-syntax-entry ?* ".")
      (modify-syntax-entry ?+ ".")
      (modify-syntax-entry ?/ ".")
      (let* ((sym (symbol-name (symbol-at-point)))
             (parts (split-string sym "\\."))
             (newsym (concat (car parts) "." (concat "_" (cadr parts))))
             (regexp (concat "\\_<\\(" (regexp-quote sym) "\\)\\_>")))
        (save-excursion
          (goto-char (point-min))
          (let ((case-fold-search nil))
            (while (re-search-forward regexp nil t)
              (replace-match newsym t nil))))))))

(setq realgud-safe-mode nil)

(defun ruin/doxymacs--enter-insert (&rest _ignore)
  (evil-append 0))

(add-function :after (symbol-function 'doxymacs-call-template) #'ruin/doxymacs--enter-insert)

(setq tempo-interactive t)

(provide 'ruin-lua)
