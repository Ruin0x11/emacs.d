;;; ruin-mail.el --- mail

(when (file-exists-p "/usr/share/emacs/site-lisp/notmuch.el")
  (require 'notmuch))

(setq
 ;; setup the mail address and use name
 mail-user-agent 'message-user-agent
 user-mail-address "ipickering2@gmail.com"
 user-full-name "Ian Pickering"
 ;; smtp config
 smtpmail-smtp-server "smtp.gmail.com"
 message-send-mail-function 'message-smtpmail-send-it

 ;; report problems with the smtp server
 smtpmail-debug-info t
 ;; add Cc and Bcc headers to the message buffer
 message-default-mail-headers "Cc: \nBcc: \n"
 ;; postponed message is put in the following draft directory
 message-auto-save-directory "~/mail/draft"
 message-kill-buffer-on-exit t
 ;; change the directory to store the sent mail
 message-directory "~/mail/")

(defun notmuch-exec-offlineimap ()
  "execute offlineimap"
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "offlineimap"
                                "*offlineimap*"
                                "offlineimap -o")
   '(lambda (process event)
      (notmuch-refresh-all-buffers)
      (let ((w (get-buffer-window "*offlineimap*")))
        (when w
          (with-selected-window w (recenter (window-end)))))))
  (popwin:display-buffer "*offlineimap*"))

(add-to-list 'popwin:special-display-config
             '("*offlineimap*" :dedicated t :position bottom :stick t
               :height 0.4 :noselect t))

(provide 'ruin-mail)
