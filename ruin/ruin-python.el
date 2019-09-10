(require 'package)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-require 'elpy)
(package-initialize)
(elpy-enable)

(package-require 'company-jedi)
(package-require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(setq python-shell-interpreter "python3")

(eval-after-load 'python '(progn
                            (add-to-list 'evil-emacs-state-modes 'inferior-python-mode)
                            (delete 'inferior-python-mode evil-insert-state-modes)))

(evil-leader/set-key-for-mode 'python-mode
  "dd" 'jedi:show-doc
  "mi" 'run-python
  "mr" 'python-shell-switch-to-shell)


(provide 'ruin-python)
