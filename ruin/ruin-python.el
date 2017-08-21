(require 'package)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-require 'elpy)
(package-initialize)
(elpy-enable)

(setq python-shell-interpreter "python3")

(eval-after-load 'python '(progn
                            (add-to-list 'evil-emacs-state-modes 'inferior-python-mode)
                            (delete 'inferior-python-mode evil-insert-state-modes)))

(evil-leader/set-key-for-mode 'python-mode
  "mpr" 'run-python
  "mpz" 'python-shell-switch-to-shell)


(provide 'ruin-python)
