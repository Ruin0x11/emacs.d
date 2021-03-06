;;;ruin-misc-modes.el --- modes too small for individual .el files

;;; semantic
;(semantic-mode)
;(global-semantic-decoration-mode)
;(global-semantic-stickyfunc-mode)
;(global-semantic-highlight-func-mode)
;(global-semantic-show-parser-state-mode)
;(global-semantic-highlight-edits-mode)

; (evil-leader/set-key
;   "fj" 'semantic-ia-fast-jump
;   "fu" 'senator-go-to-up-reference
;   "fy" 'semantic-symref-symbol)
(add-to-list 'evil-emacs-state-modes 'semantic-symref-results-mode)
(eval-after-load "semantic/list" #'(lambda ()
                              (define-key semantic-symref-results-mode-map (kbd "C-u") 'evil-scroll-up)
                              (define-key semantic-symref-results-mode-map (kbd "C-d") 'evil-scroll-down)
                              (define-key semantic-symref-results-mode-map "j" 'evil-next-line)
                              (define-key semantic-symref-results-mode-map "k" 'evil-previous-line)))


;;; winner
(winner-mode)


;;; which-key
(package-require 'which-key)
(setq which-key-idle-delay 0.2)
(require 'which-key)
(which-key-mode)


;;; request
(package-require 'request)


;;; persp
; (package-require 'persp-mode)
; (with-eval-after-load "persp-mode"
;   (setq wg-morph-on nil)
;   (persp-mode)
;   (persp-frame-switch "base")
;
;   (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
; (require 'persp-mode)
;
(setq persp-auto-resume-time 0
      persp-auto-save-opt 2
      persp-nil-name "base"
      persp-kill-foreign-buffer-behaviour 'kill)
; (run-with-idle-timer 30 t (lambda () (let ((inhibit-message nil)) (persp-save-state-to-file))))
;
; (set-face-background 'persp-face-lighter-default "magenta")
;
; (evil-leader/set-key
;   "sn" 'persp-next
;   "sp" 'persp-prev
;   "ss" 'persp-frame-switch
;   "sK" 'persp-kill
;   "sw" 'persp-save-state-to-file
;   "sl" 'persp-load-state-from-file
;   "sr" 'persp-rename)

;;; electric-indent
(electric-indent-mode 1)


;;; savehist
(add-hook 'after-init-hook 'savehist-mode)

(setq savehist-file "~/.emacs.d/savehist")


;;; quickrun
(package-require 'quickrun)

(setq quickrun-timeout-seconds nil
      quickrun-focus-p nil)

(add-hook 'quickrun-after-run-hook (lambda ()
                                     (quickrun/recenter -5)))
(add-hook 'quickrun/mode-hook      (lambda ()
                                     (quickrun/recenter -5)))

(require 'quickrun)
;; delete active quickrun window and buffer
(defun quickrun/kill-quickrun-buffer ()
  (when (get-buffer quickrun/buffer-name)
    (if (window-live-p (get-buffer-window quickrun/buffer-name))
        (delete-window (get-buffer-window quickrun/buffer-name)))
    (kill-buffer quickrun/buffer-name)))


;;; lookup
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/lookup")
                                        ; (load "lookup-autoloads")
                                        ; (evil-leader/set-key
                                        ; "ll" 'lookup
                                        ; "lw" 'lookup-word
                                        ; "lp" 'lookup-pattern)

                                        ;(load "lookup-autoloads")
                                        ;(setq lookup-mecab-coding-system 'utf-8)
                                        ;(setq lookup-search-agents '(;;(ndmecab)
                                        ;                            (ndict "dict.us.dict.org")
                                        ;                            (ndsary "~/dicts")
                                        ;                            ))
                                        ; (add-to-list 'evil-emacs-state-modes 'lookup-select-mode)
                                        ; (add-to-list 'evil-emacs-state-modes 'lookup-history-mode)
                                        ; (add-to-list 'evil-emacs-state-modes 'lookup-content-mode)
                                        ; (add-to-list 'evil-emacs-state-modes 'lookup-modules-mode)
                                        ; (add-to-list 'evil-emacs-state-modes 'lookup-summary-mode)


;;;###autoload
(defun lookup-region-noconfirm (beg end &optional mod)
  "Search for the region."
  (interactive (lookup-region-input))
  (let* ((lookup-edit-input nil))
    (lookup-word (buffer-substring-no-properties beg end) mod)))
(global-set-key (kbd "C-c C-l") 'lookup-region-noconfirm)


;;; expand-region
(package-require 'expand-region)
(define-key evil-normal-state-map (kbd "C-'") 'er/expand-region)
(define-key evil-visual-state-map (kbd "C-'") 'er/expand-region)
(define-key evil-normal-state-map (kbd "C-\"") 'er/contract-region)
(define-key evil-visual-state-map (kbd "C-\"") 'er/contract-region)

(defun get-lookup-entries (query)
  (let ((query (lookup-new-query lookup-default-method query))
        (entries '()))
    (dolist (dict (or lookup-search-dictionaries
                      (lookup-module-dictionaries (lookup-default-module))))
      (setf entries (append entries (lookup-dictionary-search dict query))))
    entries))


;;; anzu
(package-require 'anzu)
(package-require 'evil-anzu)
(global-anzu-mode 1)
(setq anzu-cons-mode-line-p t)
(with-eval-after-load 'evil
  (require 'evil-anzu))


;;; arduino-mode
(package-require 'arduino-mode)
(setq auto-mode-alist (remove (rassoc 'arduino-mode auto-mode-alist) auto-mode-alist))


;;; processing-mode
(package-require 'processing-mode)
(setq processing-location "/usr/bin/processing-java"
      processing-application-dir "/usr/bin/processing"
      processing-sketchbook-dir "/home/ruin/sketchbook")
(evil-leader/set-key-for-mode 'processing-mode
  "mr" 'processing-sketch-run)

(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))
(add-to-list 'auto-mode-alist '("\\.pde\\'" . processing-mode))

;; Quickrun for processing-mode
(quickrun-add-command "processing"
  '((:command . "/usr/bin/processing-java")
    (:exec    . "%c --force --sketch=%d --run --output=%d/output")
    (:tempfile . nil))
  :mode 'processing-mode)


;;; sos
(require 'sos)


;;; crux
(package-require 'crux)
(evil-leader/set-key
  "fw" 'crux-view-url)


;;; lively
(require 'lively)
(evil-define-key 'insert emacs-lisp-mode-map (kbd "C-M-l") 'lively)

(package-require 'google-translate)
(setq google-translate-default-target-language "en")
(evil-leader/set-key
  "at" 'google-translate-at-point)


;;; markdown-mode
(package-require 'markdown-mode)
(require 'markdown-mode)
(eval-after-load "markdown-mode" #'(lambda ()
                                     (define-key markdown-mode-map (kbd "<C-return>") 'markdown-follow-thing-at-point)))
(add-hook 'markdown-mode-hook #'flyspell-mode)

(setq markdown-gfm-use-electric-backquote nil
      markdown-fontify-code-blocks-natively t)
(global-evil-leader-mode t)
(set-face-attribute 'markdown-code-face nil :background "#3a3a3a")

(package-require 'writeroom-mode)
(require 'downdraft)
(downdraft-add-to-mode-line)

(setq writeroom-mode-line '(downdraft-mode-line-string downdraft-mode-line-string)
      writeroom-maximize-window nil
      downdraft-default-goal-time 5
      downdraft-default-goal-word-count 800)

; (add-to-list 'markdown-mode-hook 'writeroom-mode)

;; (add-hook 'compilation-shell-minor-mode-hook
;;           #'(lambda ()
;;               (setq compilation-scroll-output nil)))

(setq feature-cucumber-command "bundle exec rake cucumber CUCUMBER_OPTS=\"{options} -r features\" FEATURE=\"{feature}\"")

(ruin/set-shift-width-for-mode 'feature-mode-hook 'feature-indent-offset)

(add-hook 'feature-mode-hook '(lambda ()
                                (local-set-key (kbd "RET") 'newline-and-indent)))

;;; YAML
(package-require 'yaml-mode)
(add-hook 'yaml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq evil-shift-width 2)))
(add-to-list 'auto-mode-alist '("\\.clang-format$" . yaml-mode))


;;; kaomoji
(package-require 'kaomoji)
(require 'kaomoji)
(defun my-slurp (file)
  (with-temp-buffer
    (insert-file-contents (locate-user-emacs-file file))
    (goto-char (point-min))
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun facemark-kaomoji ()
  (mapcar (lambda (st)
            (let* ((pair (butlast (split-string st "\t" t)))
                   (key (first pair))
                   (value (second pair)))
              `((,key) . ,value)))
          (split-string (my-slurp
                         (locate-user-emacs-file "site-lisp/list.txt")) "\n" t)))

(setq kaomoji-table (append kaomoji-table (facemark-kaomoji)))
(setq kaomoji-candidates-limit 50)

(evil-leader/set-key
  "ak" 'kaomoji)

;;; google-this
(package-require 'google-this)
(evil-leader/set-key
  "ag" 'google-this
  "aG" (lambda () (interactive) (google-this-line nil t))
  "al" 'google-this-lucky-search)


;;; doc-mode
(require 'doc-mode)
(add-hook 'c-mode-common-hook 'doc-mode)
(add-hook 'java-mode-hook 'doc-mode)
(evil-leader/set-key-for-mode 'java-mode
  "mdd" 'doc-mode-fix-tag-doc)


;;; hsp-mode
(require 'hsp-mode)
(evil-leader/set-key
  "fd" 'xref-find-definitions
  "fg" 'xref-find-references)

(evil-define-key 'normal hsp-mode-map (kbd "M-.") 'xref-find-definitions)
(evil-define-key 'normal hsp-mode-map (kbd "M-?") 'xref-find-references)

(defun ruin/hsp-etags-eldoc-function ()
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
            (delay-mode-hooks (hsp-mode))
            (font-lock-default-function 'hsp-mode)
            (font-lock-default-fontify-region (point-min)
                                              (point-max)
                                              nil)
            (buffer-string)))))))

(add-hook 'hsp-mode-hook
          (lambda ()
            (eldoc-mode)
            (setq-local eldoc-documentation-function 'ruin/etags-eldoc-function)))

(setq hsp-imenu-generic-expression
  '(("Function"  "^\\s *#def[c]?func\\s +\\(?:[^( \t\n.]*\\.\\)?\\([^( \t\n]+\\)" 1)
    ("Label"  "^\\s *\\*\\(?:[^( \t\n.]*\\.\\)?\\([^( \t\n]+\\)" 1)
    ))


;;; uim
(make-obsolete
 'process-kill-without-query
 "use `process-query-on-exit-flag' or `set-process-query-on-exit-flag'."
 "22.1")
(defun process-kill-without-query (process &optional flag)
  "Say no query needed if PROCESS is running when Emacs is exited.
Optional second argument if non-nil says to require a query.
Value is t if a query was formerly required."
  (let ((old (process-query-on-exit-flag process)))
    (set-process-query-on-exit-flag process nil)
    old))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/uim-el")
(if (locate-library "uim") (require 'uim))
(global-set-key (kbd "C-\\") 'uim-mode)
(setq uim-default-im-engine "anthy")

;;; buffer-move
(package-require 'buffer-move)
(global-set-key (kbd "C-S-k") 'buf-move-up)
(global-set-key (kbd "C-S-j") 'buf-move-down)
(global-set-key (kbd "C-S-h") 'buf-move-left)
(global-set-key (kbd "C-S-l") 'buf-move-right)

;;; command-frequency
(package-require 'keyfreq)
(keyfreq-mode)
(keyfreq-autosave-mode)
(evil-leader/set-key "af" 'keyfreq-show)


;;; highlight-symbol
(package-require 'highlight-symbol)
(defun ruin/highlight-evil-search ()
  (interactive)
  (highlight-symbol (evil-get-register ?/))
  (evil-ex-nohighlight))

(evil-leader/set-key
  "HH" 'highlight-symbol-at-point
  "Hr" 'highlight-symbol-remove-all
  "Hs" 'highlight-regexp
  "Hc" 'highlight-symbol-remove-all
  "H/" 'ruin/highlight-evil-search)
;;; glsl-mode
(package-require 'glsl-mode)
(defun ruin/open-this-file-in-shader-view ()
  (interactive)
  (shell-command-on-file "glslViewer"))
(add-to-list 'auto-mode-alist '("\\.fs$" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vs$" . glsl-mode))


;;; compilation-shell-minor-mode
;; Can't be without it.
(add-hook 'compilation-mode-hook 'compilation-shell-minor-mode)
(add-hook 'shell-hook 'compilation-shell-minor-mode)


;;; dumb-jump
(package-require 'dumb-jump)
; (dumb-jump-mode)
; (evil-leader/set-key
;   "fj" 'dumb-jump-go
;   "fp" 'dumb-jump-back)


;;; mpc
(require 'mpc)


;;; firestarter
(package-require 'firestarter)
(firestarter-mode)
(setq firestarter-default-type 'finished)

(put 'firestarter 'safe-local-variable 'identity)


;;; restclient
(package-require 'restclient)

(defun ruin/start-restclient ()
  (interactive)
  (delete-other-windows)
  (let ((exists (member "*restclient*" (mapcar 'buffer-name (buffer-list)))))
    (switch-to-buffer "*restclient*")
    (when (not exists)
      (ruin/insert-template "httpbin")
      (setq-local url-max-redirections 0)))
  (restclient-mode))

(evil-leader/set-key
  "ar" 'ruin/start-restclient)


;;; howdoi
(package-require 'howdoi)


;;; edbi
(defun ruin/start-edbi (uri &optional username password)
  "Open Database viewer buffer with args."
  (interactive "sUri: ")
  (if (or (null uri)
          (string-equal "" uri))
      (error "Uri cannot be empty")
    (let* ((connection-func
           (lambda (ds)
             (let (conn msg)
               (setq msg
                     (condition-case err
                         (progn
                           (setq conn (edbi:start))
                           (edbi:connect conn ds)
                           nil)
                       (error (format "%s" err))))
               (cond
                ((null msg)
                 (deferred:call 'edbi:dbview-open conn) nil)
                (t msg)))))
          (msg (funcall connection-func (edbi:data-source uri username password))))
      (when msg (error (format "Connection error: %s" msg))))))

;; (evil-leader/set-key
;;   "ae" 'ruin/start-edbi)

;;; powershell
(package-require 'powershell)
(require 'powershell)

(when (memq system-type '(gnu/linux darwin))
  (setq powershell-location-of-exe "pwsh"))

(defvar powershell-cmdlet-cache nil)

(defun powershell-symbol-at-point (&optional look-back)
  "Return the name of the symbol at point, otherwise nil.
If LOOK-BACK is non-nil, move backwards trying to find a symbol
if there isn't one at point."
  (or (when-let ((str (thing-at-point 'symbol)))
        (unless (text-property-any 0 (length str) 'field 'cider-repl-prompt str)
          (substring-no-properties str)))
      (when look-back
        (save-excursion
          (ignore-errors
            (while (not (looking-at "\\sw\\|\\s_\\|\\`"))
              (forward-sexp -1)))
          (powershell-symbol-at-point)))))

(defun powershell-format-cmd (command)
  (concat powershell-location-of-exe " -NoProfile -c \"" command "\""))

(defun powershell-run-async (command)
  (interactive "scommand: ")
  (call-process-shell-command (powershell-format-cmd command)))

(defun powershell-run-sync (command)
  (interactive "scommand: ")
  (message (shell-command-to-string (powershell-format-cmd command))))

(defun powershell-run-cmd (command symbol)
  "Run COMMAND for the PowerShell symbol SYMBOL."
  (if symbol
        (let* ((cmd (powershell-format-cmd command))
              (formatted (replace-regexp-in-string "\%s" symbol cmd)))
          (shell-command-to-string formatted))
      (user-error "No symbol found")))

(defun powershell-read-symbol (rehash look-back)
  "Read a PowerShell symbol interactively or at point.

If REHASH is set, rehashes the list of all cached cmdlets."
  (let* ((symbol (powershell-symbol-at-point look-back))
         (prompt (concat "Find symbol"
                         (and symbol (format " (default %s)" symbol))
                         ": "))
         (enable-recursive-minibuffers t))
    (completing-read
     prompt
     (powershell-all-cmdlets rehash)
     nil
     t
     symbol)))

(defun powershell-doc (&optional topic rehash)
  "Lookup PowerShell documentation."
  (interactive (list nil current-prefix-arg))
  (let ((topic (or topic
                   (powershell-read-symbol nil nil)))
         (buffer-name (format "*PowerShell Get-Help*")))
    (let ((buffer (get-buffer-create buffer-name))
            (content (powershell-run-cmd "Get-Help %s -full" topic)))
        (with-current-buffer buffer
          (erase-buffer)
          (insert content)
          (goto-char (point-min))))
    (display-buffer buffer-name)))

(defun powershell-all-cmdlets (rehash)
  "Return a string with all PowerShell cmdlets."
  (if (or rehash (null powershell-cmdlet-cache))
      (let* ((command (concat powershell-location-of-exe " -NoProfile -c \"Get-Command | select -Property Name\""))
             (str (shell-command-to-string command))
             (cmds (seq-drop (split-string str "\n" t "[ ]+") 2)))
        (setq powershell-cmdlet-cache cmds))
    powershell-cmdlet-cache))

(defvar powershell-helm-cmdlet-docs
 (helm-build-sync-source "test"
   :candidates (powershell-all-cmdlets nil)
   :action (helm-make-actions "Lookup" 'powershell-doc))
 "Source for looking up PowerShell documentation.")

;;;###autoload
(defun powershell-helm-docs ()
  "Search through all PowerShell documentation with Helm."
  (interactive)
  (helm :sources '(powershell-helm-cmdlet-docs)
        :buffer "*PowerShell Docs*"
        :prompt "Doc: "))

(evil-leader/set-key-for-mode 'powershell-mode
  "dd" 'powershell-doc
  "df" 'powershell-helm-docs)

;;; Outshine

(package-require 'outshine)
(require 'outshine)
(add-hook 'outline-minor-mode-hook 'outshine-mode)
(defun -add-font-lock-kwds (FONT-LOCK-ALIST)
  (font-lock-add-keywords
   nil (--map (-let (((rgx uni-point) it))
                `(,rgx (0 (progn
                            (compose-region (match-beginning 1) (match-end 1)
                                            ,(concat "\t" (list uni-point)))
                            nil))))
              FONT-LOCK-ALIST)))

(defmacro add-font-locks (FONT-LOCK-HOOKS-ALIST)
  `(--each ,FONT-LOCK-HOOKS-ALIST
     (-let (((font-locks . mode-hooks) it))
       (--each mode-hooks
         (add-hook it (-partial '-add-font-lock-kwds
                                (symbol-value font-locks)))))))

(defconst emacs-outlines-font-lock-alist
  ;; Outlines
  '(("\\(^;;;\\) "          ?■)
    ("\\(^;;;;\\) "         ?○)
    ("\\(^;;;;;\\) "        ?✸)
    ("\\(^;;;;;;\\) "       ?✿)))

(add-font-locks
 '((emacs-outlines-font-lock-alist emacs-lisp-mode-hook)))

;;; auto-YASnippet
(package-require 'auto-yasnippet)

;;; diminish
(package-require 'diminish)
(eval-after-load "helm" '(diminish 'helm-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "flycheck" '(diminish 'flycheck-mode))
(eval-after-load "evil-commentary" '(diminish 'evil-commentary-mode))
;; (eval-after-load "paredit" '(diminish 'paredit-mode))
;; (eval-after-load "autopair" '(diminish 'autopair-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))
;; (eval-after-load "highlight-parentheses" '(diminish 'highlight-parentheses-mode))
;; (eval-after-load "subword" '(diminish 'subword-mode))
(eval-after-load "anzu" '(diminish 'anzu-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "magit" '(diminish 'magit-auto-revert-mode))
(eval-after-load "abbrev" '(diminish 'abbrev-mode))
(eval-after-load "evil-smartparens" '(diminish 'evil-smartparens-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "autorevert" '(diminish 'auto-revert-mode))
(eval-after-load "ruby-block" '(diminish 'ruby-block-mode))
; (eval-after-load "persp-mode" '(diminish 'persp-mode))
(eval-after-load "whitespace" '(diminish 'global-whitespace-mode))
(eval-after-load "org-indent" '(diminish 'org-indent-mode))
(eval-after-load "evil-org" '(diminish 'evil-org-mode))
(eval-after-load "prettier-js" '(diminish 'prettier-js-mode))
(eval-after-load "doc-mode" '(diminish 'doc-mode))
(eval-after-load "minitest-mode" '(diminish 'doc-mode))
(eval-after-load "doxymacs" '(diminish 'doxymacs-mode))
(eval-after-load "doxymacs" '(diminish 'doxymacs-mode))
(eval-after-load "rainbow-mode" '(diminish 'rainbow-mode))
(eval-after-load "lua-block" '(diminish 'lua-block-mode))
(eval-after-load "ivy" '(diminish 'ivy-mode))
(eval-after-load "outshine" '(diminish 'outshine-mode))
(eval-after-load "lispyville" '(diminish 'lispyville-mode))
(eval-after-load "outline" '(diminish 'outline-mode))
;(eval-after-load "rainbow-mode" '(diminish 'rainbow-mode))

(diminish 'compilation-in-progress "㋙")
(diminish 'visual-line-mode)

;;; hcl-mode
(package-require 'hcl-mode)
(setq hcl-indent-level 4)
(add-hook 'hcl-mode-hook (lambda ()
                           (define-key hcl-mode-map (kbd "RET") 'indent-new-comment-line)
                           (yas-minor-mode-on)
                           (setq-local fill-column 100)))

;;; open-paren-modes
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(setq open-paren-modes
      '(rust-mode glsl-mode c-mode c++-mode hcl-mode lua-mode org-mode css-mode))

(dolist (mode open-paren-modes)
  (sp-local-pair mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET"))))

(add-hooks open-paren-modes 'smartparens-mode)
(setq sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil
      sp-highlight-wrap-tag-overlay nil)

(package-require 'hydra)

(defun w32-run (name)
  (call-process-shell-command (concat "START " name)))

(defun control-panel (&optional system)
  "Run the control panel for SYSTEM."
  (let ((cmd (if system (concat "control system") "control")))
    (w32-run cmd)))

(defun elevated-cmd ()
  "Start an elevated command prompt in the current buffer's directory.

If no directory is associated with the buffer, \"C:\\\" is used
instead."
  (interactive)
  (let* ((dir (if (buffer-file-name)
                  (replace-regexp-in-string "/" "\\\\\\\\"
                                           (file-name-directory (buffer-file-name)))
                "C:\\\\"))
         (cmd (concat "Start-Process cmd -ArgumentList \\\"/K\\\",\\\"cd " dir "\\\" -Verb runAs")))
    (powershell-run-sync cmd)))

(defhydra windows-shortcuts-hydra nil
  "Windows"
  ("d" (w32-run "shell:Downloads") "Downloads")
  ("o" (w32-run "shell:DocumentsLibrary") "Documents")
  ("m" (w32-run "shell:MyComputerFolder") "My Computer")
  ("x" (explorer) "Explorer")

  ("a" (call-process-shell-command "\"C:\\Windows\\System32\\SystemPropertiesAdvanced.exe\"") "System Properties")

  ("c" (elevated-cmd) "cmd")
  ("t" (w32-run "Taskschd.msc") "Task Scheduler")
  ("r" (control-panel) "Control Panel")
  ("s" (control-panel "sysdm.cpl") "System")

  ("q" nil "quit")
  )

(when (eq system-type 'windows-nt)
  (evil-leader/set-key "hw" 'windows-shortcuts-hydra/body))

;;; GPG
(require 'epa)

;;; lsp-mode
(package-require 'lsp-mode)
(require 'lsp-mode)
(setq lsp-print-io nil
      lsp-response-timeout 20000
      lsp-document-sync-method 'incremental
      company-lsp-async nil
      company-lsp-enable-snippet t)

(defun ruin/lsp-mode-toggle-print-io ()
  (interactive)
  (setq lsp-print-io (not lsp-print-io)))

(defun lsp--render-on-hover-content (contents render-all)
  "Render the content received from 'document/onHover' request.
CONTENTS  - MarkedString | MarkedString[] | MarkupContent
RENDER-ALL - nil if only the signature should be rendered."
  (if (and (hash-table-p contents) (gethash "kind" contents))
      ;; MarkupContent, deprecated by LSP but actually very flexible.
      ;; It tends to be long and is not suitable in echo area.
      (if render-all (lsp--render-element contents) "")
    ;; MarkedString -> MarkedString[]
    (when (or (hash-table-p contents) (stringp contents))
      (setq contents (list contents)))
    ;; Consider the signature consisting of the elements who have a renderable
    ;; "language" property. When render-all is nil, ignore other elements.
    (string-join
     (seq-map
      #'lsp--render-element
      (if render-all
          (reverse contents)
        (--filter (and (hash-table-p it) (lsp-get-renderer (gethash "language" it)))
                  contents)))
     "\n\n")))

(defcustom lsp-imenu-filtered-symbols '("Other")
  "Symbols to ignore when running imenu.")

(defun lsp--symbol-filter (sym)
  "Determine if SYM is for the current document."
  ;; It's a SymbolInformation or DocumentSymbol, which is always in the current
  ;; buffer file.
  (or
   (equal (lsp--get-symbol-type sym) "Other")
   (when-let (location (gethash "location" sym))
     (not (eq (find-buffer-visiting (lsp--uri-to-path (gethash "uri" (gethash "location" sym))))
              (current-buffer))))))

(defun lsp-rename (newname)
  "Rename the symbol (and all references to it) under point to NEWNAME."
  (interactive (list (let ((symbol (lsp--get-symbol-to-rename)))
                       (read-string (format "Rename %s to: " symbol) ""))))
  (lsp--cur-workspace-check)
  (unless (lsp--capability "renameProvider")
    (signal 'lsp-capability-not-supported (list "renameProvider")))
  (let ((edits (lsp-request "textDocument/rename"
                            `(:textDocument ,(lsp--text-document-identifier)
                                            :position ,(lsp--cur-position)
                                            :newName ,newname))))
    (when edits
      (lsp--apply-workspace-edit edits))))

;; override semantic/bovine, which is taken in by doc-mode
(defvar-mode-local c-mode imenu-create-index-function 'lsp--imenu-create-index
  "Imenu index function for C.")

;(require 'lsp-imenu)
;(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)


;;; lsp-intellij
(package-require 'kotlin-mode)
(setq kotlin-tab-width 4)
(when (file-exists-p "E:/build/lsp-intellij/lsp-intellij.el")
  (load "E:/build/lsp-intellij/lsp-intellij.el")
  (with-eval-after-load 'lsp-mode
    (require 'lsp-intellij)
    (add-hook 'java-mode-hook #'lsp-intellij-enable)
    (add-hook 'kotlin-mode-hook #'lsp-intellij-enable))
  (package-require 'lsp-ui)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

; (add-hook 'lsp-after-diagnostics-hook (lambda () (message "Got diagnostics!")))

;;; Tramp
(setq putty-directory "C:\\Program Files\\PuTTY")
(when (eq window-system 'w32)
  (setq tramp-default-method "plink")
  (when (and (not (string-match putty-directory (getenv "PATH")))
	     (file-directory-p putty-directory))
    (setenv "PATH" (concat putty-directory ";" (getenv "PATH")))
    (add-to-list 'exec-path putty-directory)))


(setq eww-search-prefix "http://www.google.com/search?btnI=Im+Feeling+Lucky&q=")
(package-require 'cmake-mode)
(require 'cmake-mode)

(package-require 'actionscript-mode)

(package-require 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

(provide 'ruin-misc-modes)

(let ((uim-file "/usr/share/emacs/site-lisp/uim-el/uim.el"))
  (when (file-exists-p uim-file)
    (defun process-kill-without-query (process &optional flag)
      "Say no query needed if PROCESS is running when Emacs is exited.
Optional second argument if non-nil says to require a query.
Value is t if a query was formerly required."
      (let ((old (process-query-on-exit-flag process)))
        (set-process-query-on-exit-flag process nil)
        old))

    (add-to-list 'load-path "/usr/share/emacs/site-lisp/uim-el")
    (load uim-file)
    (global-set-key "\C-\\" 'uim-mode)
    (setq uim-default-im-engine "anthy")))

;;; midnight
(require 'midnight)
(midnight-mode)

;;; tiny
(package-require 'tiny)
(require 'tiny)
(tiny-setup-default)

;;; Docker
(package-require 'dockerfile-mode)

;;; crystal
(package-require 'crystal-mode)

;;; string-inflection
(package-require 'string-inflection)

;;; csv-mode
(package-require 'csv-mode)

;;; Local variables
;; Local Variables:
;; eval: (outline-minor-mode)
;; End:
