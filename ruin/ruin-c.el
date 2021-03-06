(package-require 'ggtags)
(package-require 'gxref)
(package-require 'function-args)
(package-require 'srefactor)
(package-require 'company-c-headers)
(package-require 'clang-format)
(package-require 'flycheck-clang-tidy)
;(package-require 'cmake-ide)
(package-require 'rmsbolt)
(require 'srefactor)
(require 'helm-imenu)
(require 'column-marker)
(require 'line-comment-banner)
                                        ; Add cmake listfile names to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(global-linum-mode 0)

(add-hook 'c-mode-common-hook
          (lambda ()
            (yas-minor-mode 1)
            ;(c-set-offset 'innamespace 0)
            (linum-mode 0)
            ;(c-set-offset 'substatement-open 0)
            (company-mode 1)
            (abbrev-mode 1)
            (which-function-mode 1)
            (define-key c-mode-map (kbd "RET") 'indent-new-comment-line)
            (setq compilation-skip-threshold 1
                  compilation-auto-jump-to-first-error nil
                  c-default-style "linux"
                  c-basic-offset 4
                  comment-fill "*"
                  fill-column 80)

            ;; xref--show-defs-buffer does not push marker stack
            (setq-local xref-show-definitions-function 'ruin/xref--show-defs-buffer)

            (when (derived-mode-p 'c-mode 'c++-mode)
              ;(semanticdb-enable-gnu-global-databases 'c-mode)
              ;(semanticdb-enable-gnu-global-databases 'c++-mode)
              ;(ggtags-mode 1)
              (eldoc-mode 1)
              ; (define-key c++-mode-map [(tab)]        'evil-complete-next)
              ; (define-key c++-mode-map (kbd "TAB")    'evil-complete-next)
              ; (define-key c++-mode-map (kbd "<tab>")  'evil-complete-next)
              ; (define-key c-mode-map [(tab)]        'evil-complete-next)
              ; (define-key c-mode-map (kbd "TAB")    'evil-complete-next)
              ; (define-key c-mode-map (kbd "<tab>")  'evil-complete-next)
              )))

(defun ruin/xref--show-defs-buffer (fetcher alist)
  (xref-push-marker-stack)
  (xref--show-defs-buffer fetcher alist))

; C++11 literals
(add-hook
 'c++-mode-hook
 '(lambda()
    ;; We could place some regexes into `c-mode-common-hook', but note that their evaluation order
    ;; matters.
    (font-lock-add-keywords
     nil '(;; complete some fundamental keywords
           ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
           ;; namespace names and tags - these are rendered as constants by cc-mode
           ("\\<\\(\\w+::\\)" . font-lock-function-name-face)
           ;;  new C++11 keywords
           ("\\<\\(alignof\\|offsetof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
           ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
           ;; PREPROCESSOR_CONSTANT, PREPROCESSORCONSTANT
           ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
           ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face)
           ;; hexadecimal numbers
           ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
           ;; integer/float/scientific numbers
           ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
           ;; c++11 string literals
           ;;       L"wide string"
           ;;       L"wide string with UNICODE codepoint: \u2018"
           ;;       u8"UTF-8 string", u"UTF-16 string", U"UTF-32 string"
           ("\\<\\([LuU8]+\\)\".*?\"" 1 font-lock-keyword-face)
           ;;       R"(user-defined literal)"
           ;;       R"( a "quot'd" string )"
           ;;       R"delimiter(The String Data" )delimiter"
           ;;       R"delimiter((a-z))delimiter" is equivalent to "(a-z)"
           ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)" 1 font-lock-keyword-face t) ; start delimiter
           (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\(.*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1 font-lock-string-face t)  ; actual string
           (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(.*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-keyword-face t) ; end delimiter

           ;; user-defined types (rather project-specific)
           ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
           ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
           ))
    (define-key c++-mode-map (kbd "RET") 'c-indent-new-comment-line))
 t)

(evil-define-key 'normal c++-mode-map
  (kbd "M-.") 'ggtags-find-tag-dwim)
(evil-define-key 'normal c-mode-map
  (kbd "M-.") 'ggtags-find-tag-dwim)
(add-to-list 'xref-backend-functions 'gxref-xref-backend)
(setq ggtags-highlight-tag nil)
(global-eldoc-mode 0)

(evil-define-key '(visual normal) c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(evil-define-key '(visual normal) c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)

(defun ruin/ggtags-refactor-name (&optional newsym)
  "Refactors the name at point in the current buffer with NEWSYM."
  (interactive)
  (let* ((sym (symbol-name (symbol-at-point)))
         (newsym (or newsym
                     (read-string (concat "Replace \"" sym "\" with: "))))
         (regexp (concat "\\_<\\(" (regexp-quote sym) "\\)\\_>")))
    (ggtags-query-replace sym newsym)))

(defun ruin/find-dupe-get-signature ()
  (ignore-errors
    (save-excursion
      (beginning-of-line)
      (let ((beg (point))
            (end (progn
                   (evil-find-char 1 #x28)
                   (point))))
        (strip-text-properties
         (buffer-substring beg end))))))

(defun ruin/find-dupe ()
  (interactive)
  (save-buffer)
  (let* ((sig (ruin/find-dupe-get-signature))
         (regex (concat "^" sig "("))
         (compilation-auto-jump-to-first-error nil))
    (grep (concat "grep -nH --null -r -e \"" regex "\" " (projectile-project-root) "src/**/*.hpp --exclude=\"variables.hpp\""))))

;; References w/ Role::Read
(defun ccls/references-read ()
  (interactive)
  (if (window-system)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 8))
    (lsp-find-locations "textDocument/references" '(:role 8))))

;; References w/ Role::Write
(defun ccls/references-write ()
  (interactive)
  (if (window-system)
      (lsp-ui-peek-find-custom "textDocument/references"
                               (plist-put (lsp--text-document-position-params) :role 16)))
  (lsp-find-locations "textDocument/references" '(:role 16)))

(defun ccls/caller-hierarchy ()
  (interactive)
  (ccls-call-hierarchy nil))

(defun ccls/callee-hierarchy ()
  (interactive)
  (ccls-call-hierarchy t))

(dolist (mode '(c-mode c++-mode))
  (evil-leader/set-key-for-mode mode

  ; "fd" 'ggtags-find-definition
  ; "fg" 'ggtags-find-reference
  ; "fs" 'helm-semantic-or-imenu
  ; "fe" 'ruin/ggtags-refactor-name
  "fd" 'lsp-find-definition
  "fD" 'lsp-find-declaration
  "fg" 'lsp-find-references
  "ft" 'lsp-find-type-definition
  "fs" 'helm-semantic-or-imenu
  "rr" 'lsp-rename
  "fm" 'ccls-member-hierarchy
  "fc" 'ccls/caller-hierarchy
  "fC" 'ccls/callee-hierarchy
  "fo" 'ccls/references-read
  "fO" 'ccls/references-write
  "fy" 'lsp-ui-find-workspace-symbol
  "fp" 'lsp-ui-peek-find-definitions
  "fh" 'ff-find-other-file

  "mu" 'ruin/symbol-usage-count

  "md" 'gdb
  "mgb" 'gud-break
  "mgd" 'gud-remove
  "mgf" 'gud-finish
  "mg<" 'gud-up
  "mg>" 'gud-down
  "mgr" 'gud-run
  "mgs" 'gud-step
  "mgn" 'gud-next
  "mgc" 'gud-cont
  "mgu" 'gud-until
  "mgw" 'gud-watch))

; (global-set-key (kbd "M-d") 'ruin/find-dupe)

(defun my-asm-mode-hook ()
  ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
  (local-unset-key (vector asm-comment-char))
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
  (setq tab-always-indent (default-value 'tab-always-indent)))

(add-hook 'asm-mode-hook #'my-asm-mode-hook)

;; (global-set-key (kbd "C-;") 'line-comment-banner)

(autoload 'cmake-mode "/usr/share/cmake-3.6/editors/emacs/cmake-mode.el" t)


(defun ruin/comment-function-action (_candidate &optional persistent)
  (helm-log-run-hook 'helm-goto-line-before-hook)
  (with-current-buffer helm-buffer
    (when (looking-at " ")
      (goto-char (next-single-property-change
                  (point-at-bol) 'semantic-tag nil (point-at-eol))))
    (let ((tag (get-text-property (point) 'semantic-tag)))
      (semantic-go-to-tag tag)
      (open-line 1)
      (yas/insert-snippet))))

(helm-add-action-to-source "Comment item" #'ruin/comment-function-action helm-source-imenu)

(defun my-prettify-c-block-comment (orig-fun &rest args)
  (let* ((first-comment-line (looking-back "/\\*\\s-*.*"))
         (star-col-num (when first-comment-line
                         (save-excursion
                           (re-search-backward "/\\*")
                           (1+ (current-column))))))
    (apply orig-fun args)
    (when first-comment-line
      (save-excursion
        (newline)
        (dotimes (cnt star-col-num)
          (insert " "))
        (move-to-column star-col-num)
        (insert "*/"))
      (move-to-column star-col-num) ; comment this line if using bsd style
      (insert "*") ; comment this line if using bsd style
      ))
  ;; Ensure one space between the asterisk and the comment
  (when (not (looking-back " "))
    (insert " ")))
(advice-add 'c-indent-new-comment-line :around #'my-prettify-c-block-comment)


(add-hook 'm4-mode-hook (lambda () (turn-off-smartparens-mode)))

(package-require 'hideshow)

(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-capf)
(require 'asm-mode)
(add-hook 'asm-mode-hook (lambda ()
                           (setq indent-tabs-mode nil) ; use spaces to indent
                           (electric-indent-mode -1) ; indentation in asm-mode is annoying
                           (setq tab-stop-list (number-sequence 2 60 2))))
(add-hook 'asm-mode-hook (lambda()
                           (setq tab-width 4)
                           (setq asm-indent-level 4)))

(define-key asm-mode-map (kbd "<ret>") 'newline-and-indent)
(define-key asm-mode-map (kbd "M-.") 'helm-etags-select)
(setq asm-comment-char ?\@)

(setq gdb-show-main nil
      gdb-many-windows t
      gdb-show-main t
      gdb-non-stop-setting t)

;; enable next-error/previous-error in helm-ag
(add-hook 'helm-ag-mode-hook 'grep-mode)

(defvar ruin/clang-format-buffer-on nil)

;(add-hook 'before-save-hook (lambda ()
;                              (when (and ruin/clang-format-buffer-on (derived-mode-p 'c++-mode))
;                                (clang-format-buffer))))

(let ((doxymacs-file "/usr/share/emacs/site-lisp/doxymacs.el"))
  (when (file-exists-p doxymacs-file)
    (load doxymacs-file)
    (require 'doxymacs)
    (add-hook 'c-mode-common-hook 'doxymacs-mode)))

;;(eval-after-load 'flycheck
;;  '(add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup))

; (cmake-ide-setup)
; (setq cmake-ide-flags-c++ (append '("-std=c++11")))
(delete 'company-clang company-backends)
(define-key company-active-map (kbd "C-v") 'company-next-page)
(define-key company-active-map (kbd "M-v") 'company-previous-page)

;(setq company-c-headers-path-user
;      (delete-dups
;       (mapcar #'file-name-directory
;               (directory-files-recursively "/home/ruin/build/elonafoobar/src" ""))))

(setq flycheck-clang-definitions '("SNAIL_RENDERER_SDL")
      ;cmake-ide-flags-c++ '("-I/usr/include/SDL2" "-DSNAIL_RENDERER_SDL")
      lsp-prefer-flymake nil)

; (with-eval-after-load 'ccls
;   (when (window-system)
;     (setq ccls-sem-highlight-method 'font-lock)
;     ;; (setq ccls-sem-highlight-method 'overlay)
;     (ccls-use-default-rainbow-sem-highlight)))

(package-require 'lsp-ui)
(setq lsp-eldoc-render-all t
      lsp-ui-doc-enable nil
      lsp-ui-sideline-enable nil
      lsp-ui-imenu-enable t
      lsp-ui-peek-always-show t)

(package-require 'ccls)
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook
            (lambda ()
              (require 'lsp-ui)
              (require 'ccls)
              (lsp-ui-imenu-enable t)
              (flycheck-mode 1)
              (lsp))))

(add-hook 'ld-script-mode-hook
          (lambda ()
            (setq-local comment-fill "*")))

(if (not (version< emacs-version "27"))
    (with-eval-after-load 'smartparens
      (add-to-list 'sp--special-self-insert-commands 'c-electric-paren)
      (add-to-list 'sp--special-self-insert-commands 'c-electric-brace)))

(setq doxymacs-blank-multiline-comment-template
 '(n > "/**" > n "* " p  > n " */"))

(add-to-list 'compilation-error-regexp-alist-alist
         '(elonafoobar "^                       at \\([^\n]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))
(add-to-list 'compilation-error-regexp-alist-alist '(elonafoobar-error "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(error:\\|fatal error:\\| *required from here\\)" 1 2 3)
         compilation-error-regexp-alist-alist)
(add-to-list 'compilation-error-regexp-alist-alist '(catch2 "^\\(.+\\):\\([0-9]+\\): \\(FAILED:\\)" 1 2)
         compilation-error-regexp-alist-alist)
(add-to-list 'compilation-error-regexp-alist-alist
         '(rake-test "[	 ]*\\(.*\\):\\([1-9][0-9]*\\):in" 1 2))
(add-to-list 'compilation-error-regexp-alist-alist
         '(love "^\\(Error: \\|luajit: \\)?[ \t]*\\(Syntax error: \\)?\\([\\.\\/a-zA-Z0-9_]+\\.lua\\):\\([0-9]+\\):" 3 4))
(add-to-list 'compilation-error-regexp-alist 'rake-test)
(add-to-list 'compilation-error-regexp-alist 'elonafoobar)
(add-to-list 'compilation-error-regexp-alist 'elonafoobar-error)
(add-to-list 'compilation-error-regexp-alist 'catch2)
(add-to-list 'compilation-error-regexp-alist 'catch2)
(add-to-list 'compilation-error-regexp-alist 'love)

(sp-with-modes '(c++-mode)
  (sp-local-pair "<" ">" :actions nil))

(provide 'ruin-c)
