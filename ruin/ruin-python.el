;; (package-require 'python-mode)
;; (require 'python-mode)
;; ;; (add-hook 'python-mode-hook
;; ;;           '(lambda () (eldoc-mode 1)) t)

(eval-after-load 'python '(progn
                            (add-to-list 'evil-emacs-state-modes 'inferior-python-mode)
                            (delete 'inferior-python-mode evil-insert-state-modes)))

(provide 'ruin-python)
