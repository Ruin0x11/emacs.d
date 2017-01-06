;; (package-require 'python-mode)
;; (require 'python-mode)
;; ;; (add-hook 'python-mode-hook
;; ;;           '(lambda () (eldoc-mode 1)) t)

(setq python-shell-interpreter "python3")

(eval-after-load 'python '(progn
                            (add-to-list 'evil-emacs-state-modes 'inferior-python-mode)
                            (delete 'inferior-python-mode evil-insert-state-modes)))

(evil-leader/set-key-for-mode 'python-mode
  "mpr" 'run-python
  "mpz" 'python-shell-switch-to-shell)

(provide 'ruin-python)
