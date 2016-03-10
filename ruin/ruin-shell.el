(package-require 'shell-pop)

(defvar shell-default-shell (if (eq window-system 'w32)
                                'eshell
                              'ansi-term)
  "Default shell to use in Emacs. Possible values are `eshell', `shell',
`term' and `ansi-term'.")

(defvar shell-default-position 'bottom
  "Position of the shell. Possible values are `top', `bottom' and `full'.")

(defvar shell-default-height 40
  "Height in percents for the shell window.")

(defvar shell-default-term-shell "/bin/zsh"
  "Default shell to use in `term' and `ansi-term' shells.")

(setq shell-pop-window-position shell-default-position
      shell-pop-window-height   shell-default-height
      shell-pop-term-shell      shell-default-term-shell
      shell-pop-full-span t)
(defmacro make-shell-pop-command (type &optional shell)
  (let* ((name (symbol-name type)))
    `(defun ,(intern (concat "shell-pop-" name)) (index)
       (interactive "P")
       (require 'shell-pop)
       (shell-pop--set-shell-type
        'shell-pop-shell-type
        (backquote (,name
                    ,(concat "*" name "*")
                    (lambda nil (funcall ',type ,shell)))))
       (shell-pop index))))
;; (make-shell-pop-command eshell)
;; (make-shell-pop-command shell)
;; (make-shell-pop-command term shell-pop-term-shell)
;; (make-shell-pop-command multiterm)
(make-shell-pop-command ansi-term shell-pop-term-shell)

(defun spacemacs//term-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'term-mode)
    (when (term-check-proc (current-buffer))
      (term-quit-subjob))))
(add-hook 'kill-buffer-hook 'spacemacs//term-kill-buffer-hook)

(defun ansi-term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc))
                              (delete-window))))))
(add-hook 'term-mode-hook 'ansi-term-handle-close)

(defun spacemacs/default-pop-shell ()
  "Open the default shell in a popup."
  (interactive)
  (let ((shell (if (eq 'multi-term shell-default-shell)
                   'multiterm
                 shell-default-shell)))
    (call-interactively (intern (format "shell-pop-%S" shell)))))
(evil-leader/set-key
  "'"   'spacemacs/default-pop-shell)
  
(provide 'ruin-shell)
