(require 'helm-imenu)
(require 'column-marker)
(require 'line-comment-banner)
(require 'google-c-style)
(package-require 'ggtags)
                                        ; Add cmake listfile names to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(add-hook 'c-mode-common-hook
          (lambda () (make-local-variable 'comment-fill)
            (setq comment-fill "*")))

(add-hook 'asm-mode-hook
          (lambda () (make-local-variable 'comment-fill)
            (setq comment-fill "-")))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(defun my-asm-mode-hook ()
  ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
  (local-unset-key (vector asm-comment-char))
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
  (setq tab-always-indent (default-value 'tab-always-indent)))

(add-hook 'asm-mode-hook #'my-asm-mode-hook)

;; (global-set-key (kbd "C-;") 'line-comment-banner)

(autoload 'cmake-mode "/usr/share/cmake-3.6/editors/emacs/cmake-mode.el" t)

(package-require 'csharp-mode)

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

(package-require 'csharp-mode)
(package-require 'omnisharp)
(package-require 'hideshow)
(add-hook 'csharp-mode-hook 'electric-pair-mode)
(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))

(autoload 'cmake-mode "/usr/share/cmake-3.6/editors/emacs/cmake-mode.el" t)
(add-hook 'csharpmode-hook (lambda ()
                             (push '(?< . ("< " . " >")) evil-surround-pairs-alist)))

(setq omnisharp-server-executable-path "C:\\bin\\omnisharp\\OmniSharp.exe")

;; (add-hook 'csharp-mode-hook 'omnisharp-mode)

(defun csharp-hs-forward-sexp (&optional arg)
  "I set hs-forward-sexp-func to this function.


I found this customization necessary to do the hide/show magic in C#
code, when dealing with region/endregion. This routine
goes forward one s-expression, whether it is defined by curly braces
or region/endregion. It handles nesting, too.

The forward-sexp method takes an arg which can be negative, which
indicates the move should be backward.  Therefore, to be fully
correct this function should also handle a negative arg. However,
the hideshow.el package never uses negative args to its
hs-forward-sexp-func, so it doesn't matter that this function does not
do negative numbers.

The arg can also be greater than 1, which means go forward
multiple times. This function doesn't handle that EITHER.  But
again, I haven't see that as a problem."
  (message "csharp-hs-forward-sexp, (arg %d) (point %d)..."
           (if (numberp arg) arg -1)
           (point))
  (let ((nestlevel 0)
        (mark1 (point))
        (done nil))
    (if (and arg (< arg 0))
        (message "negative arg (%d) is not supported..." arg)

      ;; else, we have a positive argument, hence move forward.
      ;; simple case is just move forward one brace
      (if (looking-at "{")
          (forward-sexp arg)

                                        ; The more complex case is dealing with a "region/endregion" block.
                                        ; We have to deal with nested regions!
        (and
         (while (not done)
           (re-search-forward "^[ \\t]*#[ \\t]*\\(region\\|endregion\\)\\b"
                              (point-max) 'move)
           (cond
            ((eobp))                    ; do nothing if at end of buffer
            ((and
              (match-beginning 1)
              ;; if the match is longer than 6 chars, we know it is "endregion"
              (if (> (- (match-end 1) (match-beginning 1)) 6)
                  (setq nestlevel (1- nestlevel))
                (setq nestlevel (1+ nestlevel))))))
           (setq done (not (and (> nestlevel 0) (not (eobp))))))
         (if (= nest 0)
             (goto-char (match-end 2))))))))

(unless (assoc 'csharp-mode hs-special-modes-alist)
  (push '(csharp-mode
                                        ; "\\(^\\s*#\\s*region\\b\\)\\|{"      ; regexp for start block DID NOT WORK
          "\\(^[ \\t]*#[ \\t]*region\\b\\)\\|{"  ; regexp for start block

                                        ; "\\(^\\s*#\\s*endregion\\b\\)\\|}"   ; regexp for end block NO WORKY!
          "\\(^[ \\t]*#[ \\t]*endregion\\b\\)\\|}"   ; regexp for end block

          "/[*/]"                                ; regexp for comment start

          csharp-hs-forward-sexp                 ; hs-forward-sexp-func
          hs-c-like-adjust-block-beginning       ;c-like adjust (1 char)
                                        ;csharp-hs-adjust-block-beginning      ;csharp adjust ?
          )
        hs-special-modes-alist))

(add-hook 'csharp-mode-hook (lambda ()
                                        ; for hide/show support
                              (hs-minor-mode 1)
                              (setq hs-isearch-open t)

                                        ; with point inside the block, use these keys to hide/show
                              (local-set-key "\C-c>"  'hs-hide-block)
                              (local-set-key "\C-c<"  'hs-show-block)
                              ))




(provide 'ruin-c)
