;;; ruin-mail.el --- mail

(if (eq system-type 'gnu/linux)
    (progn

      (add-to-list 'load-path "/usr/share/emacs//site-lisp/mu4e")
      (require 'mu4e)

      (setq
       mu4e-maildir          "~/mail"
       mu4e-drafts-folder    "/[Gmail].Drafts"
       ;; mu4e-get-mail-command "offlineimap -a Gmail && panel-mail"
       mu4e-confirm-quit     nil)))

(defun ruin/mu4e-update-and-start ()
  (interactive)
  (mu4e-update-index)
  (mu4e))

(evil-leader/set-key
  "am" 'ruin/mu4e-update-and-start
  "ac" 'calc-dispatch)

(add-hook 'mu4e-view-mode-hook (lambda () (start-process "panel-mail" nil "panel-mail")))

(provide 'ruin-mail)
