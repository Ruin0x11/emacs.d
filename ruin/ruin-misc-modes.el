;;;ruin-misc-modes.el --- modes too small for individual .el files

;; semantic
(semantic-mode)

;; winner
(winner-mode)

;; which-key
(package-require 'which-key)

(setq which-key-idle-delay 0.2)
(which-key-mode)

;; quickrun
(package-require 'quickrun)

(setq quickrun-timeout-seconds nil
      quickrun-focus-p nil)

;; dictionary
(package-require 'dictionary)
(evil-leader/set-key
  "dw" 'dictionary-lookup-definition
  "dl" 'dictionary-search)

;; multiple-cursors
(package-require 'evil-multiedit)
(global-set-key (kbd "C-'") 'evil-multiedit-match-and-next)
(global-set-key (kbd "C-\"") 'evil-multiedit-match-all)

;; expand-region
(package-require 'expand-region)
(global-set-key (kbd "C-e") '(er/expand-region))
(define-key evil-visual-state-map (kbd "C-e") 'er/expand-region)
(define-key evil-normal-state-map (kbd "C-e") 'er/expand-region)

(provide 'ruin-misc-modes)
