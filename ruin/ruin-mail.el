;;; ruin-mail.el --- mail

(if (and (eq system-type 'gnu/linux) (locate-library "mu4e"))
    (progn
      (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
      (require 'mu4e)
      (require 'smtpmail)
      (package-require 'helm-mu)

      (setq
       mu4e-maildir             "~/mail"
       mu4e-drafts-folder       "/[Gmail].Drafts"
       mu4e-sent-folder         "/[Gmail].Sent Mail"
       mu4e-trash-folder        "/[Gmail].Trash"
       mu4e-get-mail-command    "mail-sync"
       mu4e-confirm-quit     nil
       message-cite-reply-position 'above
       message-cite-style message-cite-style-gmail)

      (setq mu4e-html2text-command "/usr/bin/w3m -T text/html")

      (add-to-list 'mu4e-view-actions '("retag message" . mu4e-action-retag-message))
      (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)


      ;; something about ourselves
      (setq
       user-mail-address "ipickering2@gmail.com"
       user-full-name  "Ian Pickering"
       ;; mu4e-compose-signature
       ;; (concat
       ;;  ""
       ;;  "http://www.example.com\n")
       )
      (setq mu4e-contexts
            `(
              ,(make-mu4e-context
                :name "Main"
                :enter-func (lambda () (mu4e-message "Switch to the School context"))
                ;; leave-func not defined
                :match-func (lambda (msg)
                              (when msg
                                (mu4e-message-contact-field-matches msg
                                                                    :to "(.*)@illinois.edu")))
                :vars '((message-cite-style . message-cite-style-gmail))
                )
              ,(make-mu4e-context
                :name "School"
                :enter-func (lambda () (mu4e-message "Switch to the School context"))
                ;; leave-func not defined
                :match-func (lambda (msg)
                              (when msg
                                (mu4e-message-contact-field-matches msg
                                                                    :to "(.*)@illinois.edu")))
                :vars '((user-mail-address . "ipicker2@illinois.edu")
                        (user-full-name . "Ian Pickering")
                        (smtpmail-default-smtp-server . "smtp.illinois.edu")
                        (mu4e-compose-signature . (concat
                                                   "Ian Pickering\n"
                                                   "University of Illinois\n"))))))

      (setq message-citation-line-format "On %a, %b %d %Y, %N wrote:")

      (setq message-send-mail-function 'smtpmail-send-it
            smtpmail-stream-type 'starttls
            smtpmail-default-smtp-server "smtp.gmail.com"
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587)

      ;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
      (setq mu4e-sent-messages-behavior 'delete)

      (add-hook 'mu4e-view-mode-hook
                '(lambda () (start-process "panel-mail" nil "panel-mail"))
                'append)

      (defun ruin/mu4e-update-and-start ()
        (interactive)
        (mu4e-update-index)
        (mu4e))

      (defun ruin/open-unread-mail ()
        (interactive)
        (mu4e-update-index)
        (mu4e-headers-search "flag:unread AND NOT flag:trashed"))

      ;; don't keep message buffers around
      (setq message-kill-buffer-on-exit t)

      (evil-leader/set-key
        "aM" 'ruin/mu4e-update-and-start
        "am" 'ruin/open-unread-mail
        "aC" 'helm-mu-contacts
        "ah" 'helm-mu)
      )

(eval-after-load "mu4e-view" #'(lambda ()
                              (ruin/window-movement-for-map 'mu4e-view-mode-map)
                              (define-key mu4e-view-mode-map (kbd "C-u") 'evil-scroll-up)
                              (define-key mu4e-view-mode-map (kbd "C-d") 'evil-scroll-down)
                              (define-key mu4e-view-mode-map "j" 'evil-next-line)
                              (define-key mu4e-view-mode-map "k" 'evil-previous-line)
                              (define-key mu4e-view-mode-map "g" 'mu4e~headers-jump-to-maildir)
                              (define-key mu4e-view-mode-map "u" 'mu4e-update-index)))

(eval-after-load "mu4e-headers" #'(lambda ()
                                 (ruin/window-movement-for-map mu4e-headers-mode-map)
                                 (define-key mu4e-headers-mode-map (kbd "C-u") 'evil-scroll-up)
                                 (define-key mu4e-headers-mode-map (kbd "C-d") 'evil-scroll-down)
                                 (define-key mu4e-headers-mode-map "j" 'evil-next-line)
                                 (define-key mu4e-headers-mode-map "k" 'evil-previous-line)
                                 (define-key mu4e-headers-mode-map "g" 'mu4e~headers-jump-to-maildir)
                                 (define-key mu4e-headers-mode-map "u" 'mu4e-update-index)
                                 (define-key mu4e-headers-mode-map "J" 'mu4e-headers-next-unread)
                                 (define-key mu4e-headers-mode-map "K" 'mu4e-headers-prev-unread)))

  )

(provide 'ruin-mail)
