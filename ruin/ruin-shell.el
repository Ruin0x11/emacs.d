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
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(defun ruin/focus-emacs (orig-fn &rest args)
  (when (executable-find "i3-msg")
    (call-process "i3-msg" nil nil nil  "[class=\"Emacs\"]" "focus"))
  (apply orig-fn args))

(advice-add 'compilation-auto-jump :around #'ruin/focus-emacs)

(evil-define-key 'normal shell-mode-map (kbd "q") 'delete-window)
(evil-define-key 'emacs shell-mode-map (kbd "q") 'delete-window)
(define-key shell-mode-map (kbd "C-c C-c") (lambda () (interactive) (delete-process (buffer-name))))

(provide 'ruin-shell)
