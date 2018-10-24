(package-require 'multi-term)

(if (not (eq system-type 'windows-nt))
         (setq shell-file-name "/bin/zsh"
               multi-term-program "/bin/zsh"
               system-uses-terminfo nil
               comint-move-point-for-output nil
               comint-scroll-show-maximum-output nil))

(defun comint-esd-or-maybe-eof (arg)
  "Like `comint-delchar-or-maybe-eof' except not terrible.  Or
  maybe terrible, but in a different way."
  (interactive "p")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and (eobp)
             proc
             (= (point)
                (marker-position (process-mark proc))))
      (comint-send-eof)
      (evil-scroll-down 0))))

(add-hook 'comint-mode-hook
          (function (lambda ()
                      (local-set-key (kbd "C-d") 'comint-esd-or-maybe-eof))))
(add-hook 'inf-ruby-mode-hook
          (function (lambda ()
                      (local-set-key (kbd "C-d") 'comint-esd-or-maybe-eof))))

;(require 'ansi-color)
;(defun colorize-compilation-buffer ()
;  (toggle-read-only)
;  (ansi-color-apply-on-region compilation-filter-start (point))
;  (toggle-read-only))
;(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(defadvice term-send-raw (after clear-recorded-key activate)
  (if (string= (kbd "RET") (this-command-keys))
      (clear-this-command-keys)))

(evil-define-key 'normal shell-mode-map (kbd "q") 'delete-window)
(evil-define-key 'emacs shell-mode-map (kbd "q") 'delete-window)
(define-key shell-mode-map (kbd "C-c C-c") (lambda () (interactive) (delete-process (buffer-name))))

(provide 'ruin-shell)
