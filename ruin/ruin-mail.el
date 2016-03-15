;;; ruin-mail.el --- mail

(if (eq system-type 'gnu/linux)
    (add-to-list 'load-path "/usr/share/emacs//site-lisp/mu4e")
  (require 'mu4e)

  (setq
   mu4e-maildir          "~/mail"
   mu4e-drafts-folder    "/[Gmail].Drafts"
   mu4e-get-mail-command "offlineimap -a Gmail && panel-mail"))

(provide 'ruin-mail)
