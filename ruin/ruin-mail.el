;;; ruin-mail.el --- mail

(when (file-exists-p "/usr/share/emacs/site-lisp/notmuch.el")
  (require 'notmuch))

(require 'smtpmail)

(setq
 ;; setup the mail address and use name
 mail-user-agent 'message-user-agent
 user-mail-address "ipickering2@gmail.com"
 user-full-name "Ian Pickering"
 ;; smtp config
 smtpmail-smtp-server "smtp.gmail.com"
 message-send-mail-function 'message-smtpmail-send-it
 smtpmail-smtp-user "ipickering2@gmail.com"
 smtpmail-stream-type 'ssl
 smtpmail-mail-address "ipickering2@gmail.com"
 smtpmail-smtp-service 465

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

(defun doom-visible-windows (&optional window-list)
  "Return a list of the visible, non-popup (dedicated) windows."
  (cl-loop for window in (or window-list (window-list))
           when (or (window-parameter window 'visible)
                    (not (window-dedicated-p window)))
           collect window))

(defun ruin/notmuch-search-unread ()
  "Activate (or switch to) `notmuch' in its workspace."
  (interactive)
  (if-let* ((buf (cl-find-if (lambda (it) (string-match-p "^\\*notmuch" (buffer-name (window-buffer it))))
                             (doom-visible-windows))))
      (select-window (get-buffer-window buf))
    (notmuch-search "tag:unread")))

(defun ruin/notmuch-search-inbox ()
  "Activate (or switch to) `notmuch' in its workspace."
  (interactive)
  (if-let* ((buf (cl-find-if (lambda (it) (string-match-p "^\\*notmuch" (buffer-name (window-buffer it))))
                             (doom-visible-windows))))
      (select-window (get-buffer-window buf))
    (notmuch-search "tag:inbox")))

(evil-leader/set-key
  "amm" 'ruin/notmuch-search-unread
  "ami" 'ruin/notmuch-search-inbox
  "amc" 'notmuch-mua-mail
  "amu" 'notmuch-exec-offlineimap)

(add-to-list 'evil-normal-state-modes 'notmuch-search-mode)
(evil-define-key 'normal notmuch-search-mode-map
  "j" 'notmuch-search-next-thread
  "k" 'notmuch-search-previous-thread
  (kbd "RET") 'notmuch-search-show-thread
  "t" 'notmuch-tag-jump
  "a" 'notmuch-search-tag-all
  "gg" 'notmuch-search-first-thread
  "G" 'notmuch-search-last-thread
  "m" 'notmuch-mua-new-mail
  (kbd "C-d") 'notmuch-search-scroll-up
  (kbd "C-u") 'notmuch-search-scroll-down
  "r" 'notmuch-search-refresh-view
  "q" 'notmuch-bury-or-kill-this-buffer)

(defun ruin/notmuch-show-browse-urls ()
  "Offer to browse any URLs in the current message."
  (interactive)
  (let ((urls (notmuch-show--gather-urls)))
    (if urls
	(browse-url (completing-read "Browse URL: " (cdr urls)))
      (message "No URLs found."))))

(add-to-list 'evil-normal-state-modes 'notmuch-show-mode)
(evil-define-key 'normal notmuch-show-mode-map
  "t" 'notmuch-tag-jump
  "m" 'notmuch-mua-new-mail
  "]" 'notmuch-show-next-thread-show
  "[" 'notmuch-show-previous-thread-show
  "U" 'ruin/notmuch-show-browse-urls
  (kbd "RET") 'notmuch-show-toggle-message
  "q" 'notmuch-bury-or-kill-this-buffer)

(add-to-list 'evil-normal-state-modes 'notmuch-message-mode)
(evil-define-key 'normal notmuch-message-mode-map
  (kbd "M-q") 'fill-paragraph)

(provide 'ruin-mail)
