;;; ruin-mail.el --- mail

(if (eq system-type 'gnu/linux)
    (progn

      (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
      (require 'mu4e)
      (require 'smtpmail)

      (setq
       mu4e-maildir             "~/mail"
       mu4e-drafts-folder       "/[Gmail].Drafts"
       mu4e-sent-folder         "/[Gmail].Sent Mail"
       mu4e-trash-folder        "/[Gmail].Trash"
       mu4e-get-mail-command    "sync-mail"
       mu4e-confirm-quit     nil)

      ;; something about ourselves
      (setq
       user-mail-address "ipickering2@gmail.com"
       user-full-name  "Ian Pickering"
       ;; mu4e-compose-signature
       ;; (concat
       ;;  ""
       ;;  "http://www.example.com\n")
       )

      (setq message-send-mail-function 'smtpmail-send-it
            smtpmail-stream-type 'starttls
            smtpmail-default-smtp-server "smtp.gmail.com"
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587)

      ;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
      (setq mu4e-sent-messages-behavior 'delete)

      (defun ruin/mu4e-update-and-start ()
        (interactive)
        (mu4e-update-index)
        (mu4e))

      ;; don't keep message buffers around
      (setq message-kill-buffer-on-exit t)

      (evil-leader/set-key
        "am" 'ruin/mu4e-update-and-start))

      (add-hook 'mu4e-view-mode-hook #'(lambda () (start-process "panel-mail" nil "panel-mail"))))

(provide 'ruin-mail)
