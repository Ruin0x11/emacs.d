;;; ruin-haskell.el --- settings for Haskell
(package-require 'haskell-mode)
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
  "mi" 'haskell-process-do-info)

(add-hook 'haskell-interactive-mode-hook 'evil-insert-state)

(evil-define-key 'insert haskell-interactive-mode-map (kbd "<up>") 'haskell-interactive-mode-history-previous)
(evil-define-key 'insert haskell-interactive-mode-map (kbd "<down>") 'haskell-interactive-mode-history-next)
(evil-define-key 'insert haskell-interactive-mode-map (kbd "S-<escape>") 'haskell-interactive-switch-back)

(provide 'ruin-haskell)
