;;; ruin-haskell.el --- settings for Haskell
(package-require 'haskell-mode)
(package-require 'ghc)
(package-require 'shm)

;; Setup haskell-mode hooks
(eval-after-load "haskell-mode"
  '(custom-set-variables
    '(haskell-mode-hook
      '(haskell-indentation-mode
        haskell-doc-mode
        '(haskell-process-suggest-remove-import-lines t)
        '(haskell-process-auto-import-loaded-modules t)
        '(haskell-process-log t)
        '(haskell-interactive-popup-errors nil)
        ;; structured-haskell-mode
        ))))

;;; Evil

;; evil-open-below being dumb
;; https://github.com/syl20bnr/spacemacs/issues/3162

(evil-leader/set-key-for-mode 'haskell-mode
  "mb" 'haskell-interactive-switch
  "ml" 'haskell-process-load-file
  "mt" 'haskell-process-do-type
  "mi" 'haskell-process-do-info)

(provide 'ruin-haskell)
