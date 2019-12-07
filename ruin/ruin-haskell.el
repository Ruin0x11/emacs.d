;;; ruin-haskell.el --- settings for Haskell
(package-require 'haskell-mode)
(package-require 'flycheck-haskell)
(package-require 'ghc)
(package-require 'shm)

;; Setup haskell-mode hooks
(custom-set-variables
 '(haskell-mode-hook
  '(turn-on-haskell-indentation
    turn-on-haskell-doc))
 ;; structured-haskell-mode))
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-interactive-popup-errors nil)
 )

;; https://github.com/bodil/emacs.d/blob/master/bodil/bodil-haskell.el#L16
;; Use Unicode arrows in place of ugly ASCII arrows
;; (require 'ruin-funcs)
;; (defun setup-haskell-arrows (mode mode-map)
;;   (font-lock-replace-symbol mode "\\(->\\)" "→")
;;   (font-lock-replace-symbol mode "\\(<-\\)" "←")
;;   (font-lock-replace-symbol mode "\\(=>\\)" "⇒")

;;   (define-key mode-map (kbd "→") (lambda () (interactive) (insert "->")))
;;   (define-key mode-map (kbd "←") (lambda () (interactive) (insert "<-")))
;;   (define-key mode-map (kbd "⇒") (lambda () (interactive) (insert "=>"))))
;; (eval-after-load "haskell-mode"
;;   '(setup-haskell-arrows 'haskell-mode haskell-mode-map))

;;; Evil

;; evil-open-below being dumb
;; https://github.com/syl20bnr/spacemacs/issues/3162

(evil-leader/set-key-for-mode 'haskell-mode
  "mb" 'ruin/haskell-interactive-switch-and-move
  ;; "mL" 'haskell-process-load-file
  "ml" 'ruin/haskell-load-file-and-switch
  "mt" 'haskell-process-do-type
  "mi" 'haskell-process-do-info
  "mh" 'hoogle)

(add-to-list 'evil-emacs-state-modes 'haskell-interactive-mode)

(add-hook 'haskell-interactive-mode-hook '(lambda ()
                            (ruin/window-movement-for-map haskell-interactive-mode-map)
                            ))

(evil-define-key 'insert haskell-interactive-mode-map (kbd "<up>") 'haskell-interactive-mode-history-previous)
(evil-define-key 'insert haskell-interactive-mode-map (kbd "<down>") 'haskell-interactive-mode-history-next)
(evil-define-key 'insert haskell-interactive-mode-map (kbd "S-<escape>") 'haskell-interactive-switch-back)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(require 'haskell-mode)

;; https://github.com/haskell/haskell-mode/issues/896#issuecomment-185945576
(defun haskell-indentation-indent-line ()
  "Indent current line, cycle though indentation positions.
Do nothing inside multiline comments and multiline strings.
Start enumerating the indentation points to the right.  The user
can continue by repeatedly pressing TAB.  When there is no more
indentation points to the right, we switch going to the left."
  (interactive)
    ;; try to repeat
  (when (not (haskell-indentation-indent-line-repeat))
    (setq haskell-indentation-dyn-last-direction nil)
    ;; parse error is intentionally not caught here, it may come from
    ;; `haskell-indentation-find-indentations', but escapes the scope
    ;; and aborts the operation before any moving happens
    (let* ((cc (current-column))
           (ci (haskell-indentation-current-indentation))
           (inds (save-excursion
                   (move-to-column ci)
                   (or (haskell-indentation-find-indentations)
                       '(0))))
           (valid (memq ci inds))
           (cursor-in-whitespace (< cc ci))
           (evil-special-command? (and (bound-and-true-p evil-mode)
                                       (memq this-command '(evil-open-above
                                                            evil-open-below
                                                            evil-replace))))
           (on-last-indent (eq ci (car (last inds)))))
      (if (and valid cursor-in-whitespace)
          (move-to-column ci)
        (haskell-indentation-reindent-to
         (funcall
          (if on-last-indent
              #'haskell-indentation-previous-indentation
            #'haskell-indentation-next-indentation)
          (if evil-special-command?
              (save-excursion
                (end-of-line 0)
                (1- (haskell-indentation-current-indentation)))
            ci)
          inds
          'nofail)
         cursor-in-whitespace))
      (setq haskell-indentation-dyn-last-direction (if on-last-indent 'left 'right)
            haskell-indentation-dyn-last-indentations inds))))

(provide 'ruin-haskell)
