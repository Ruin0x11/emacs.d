;;; ruin-mail.el --- mail
(add-to-list 'load-path "/usr/share/emacs//site-lisp/mu4e")
(require 'mu4e)

(setq
  mu4e-maildir          "~/mail"
  mu4e-drafts-folder    "/[Gmail].Drafts"
  mu4e-get-mail-command "offlineimap")

(provide 'ruin-mail)
