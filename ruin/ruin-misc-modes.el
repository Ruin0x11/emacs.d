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
(define-key evil-normal-state-map (kbd "C-'") 'er/expand-region)
(define-key evil-visual-state-map (kbd "C-'") 'er/expand-region)
(define-key evil-normal-state-map (kbd "C-;") 'er/contract-region)
(define-key evil-visual-state-map (kbd "C-;") 'er/contract-region)

;; anzu
(package-require 'anzu)
(global-anzu-mode 1)

;; diminish
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
;; (eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "magit" '(diminish 'magit-auto-revert-mode))

(provide 'ruin-misc-modes)
