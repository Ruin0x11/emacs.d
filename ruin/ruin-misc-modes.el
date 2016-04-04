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

(add-hook 'quickrun-after-run-hook (lambda ()
                                (recenter 0)
                                ))

;; delete active quickrun window and buffer
(defun quickrun/kill-quickrun-buffer ()
  (when (get-buffer quickrun/buffer-name)
    (if (window-live-p (get-buffer-window quickrun/buffer-name))
        (delete-window (get-buffer-window quickrun/buffer-name)))
    (kill-buffer quickrun/buffer-name)))

;; lookup
(add-to-list 'load-path "/usr/share/emacs/site-lisp/lookup")
(load "lookup-autoloads")
(evil-leader/set-key
 "ll" 'lookup
 "lw" 'lookup-word
 "lp" 'lookup-pattern)

;; (load "lookup-autoloads")
(setq lookup-mecab-coding-system 'utf-8)
(setq lookup-search-agents '((ndmecab)
                             (ndict "dict.us.dict.org")
                             ;; (ndsary "~/dicts/JMdict")
                             ))

;; expand-region
(package-require 'expand-region)
(define-key evil-normal-state-map (kbd "C-'") 'er/expand-region)
(define-key evil-visual-state-map (kbd "C-'") 'er/expand-region)
(define-key evil-normal-state-map (kbd "C-;") 'er/contract-region)
(define-key evil-visual-state-map (kbd "C-;") 'er/contract-region)

;; anzu
(package-require 'anzu)
(global-anzu-mode 1)
(setq anzu-cons-mode-line-p nil)

;; aggressive-indent
(package-require 'aggressive-indent)
(add-hook 'emacs-lisp-hook #'aggressive-indent-mode)
(add-hook 'go-mode-hook #'aggressive-indent-mode)
(add-hook 'enh-ruby-mode-hook #'aggressive-indent-mode)

;; arduino-mode
(package-require 'arduino-mode)
(setq auto-mode-alist (remove (rassoc 'arduino-mode auto-mode-alist) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))
(add-to-list 'auto-mode-alist '("\\.pde\\'" . processing-mode))

;; processing-mode
(package-require 'processing-mode)
(setq processing-location "/usr/bin/processing-java"
      processing-application-dir "/usr/bin/processing"
      processing-sketchbook-dir "/home/ruin/sketchbook")
(evil-leader/set-key-for-mode 'processing-mode
  "mr" 'processing-sketch-run)

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
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "magit" '(diminish 'magit-auto-revert-mode))
(eval-after-load "abbrev" '(diminish 'abbrev-mode))

(provide 'ruin-misc-modes)
