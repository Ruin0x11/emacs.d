;;; ruin-helm.el --- helm settings

(package-require 'helm)
(package-require 'helm-ag)
(package-require 'projectile)
(package-require 'helm-projectile)
(package-require 'which-key)
(package-require 'quickrun)
(require 'helm-config)

(setq which-key-idle-delay 0.2)
(which-key-mode)

(setq helm-input-idle-delay 0.1)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-quick-update t
      helm-bookmark-show-location t
      helm-buffers-fuzzy-matching t
      helm-candidate-number-limit 50
      )

(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode t)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(projectile-global-mode)
(helm-projectile-on)

(defun prev-window ()
  (interactive)
  (other-window -1))

(setq quickrun-timeout-seconds nil)

(add-hook 'quickrun-after-run-hook 'prev-window)

(provide 'ruin-helm)
