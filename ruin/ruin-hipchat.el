;; ruin-hipchat.el
(package-require 'request)
(package-require 'jabber)
(require 'jabber)
(require 'jabber-hipchat)

(setq jabber-account-list '(("370715_3923118@chat.hipchat.com")))

(setq jabber-history-enabled t
      jabber-use-global-history nil
      jabber-backlog-number 40
      jabber-backlog-days 30
      jabber-libnotify-timeout 5000)

(evil-leader/set-key
 "jc" 'jabber-connect
 "jd" 'jabber-disconnect
 "jj" 'hipchat-join
 "jw" 'jabber-chat-with)

(add-hook 'jabber-alert-message-hooks 'jabber-message-libnotify) 

(add-to-list 'evil-emacs-state-modes 'jabber-chat-mode)
(ruin/window-movement-for-mode "jabber" 'jabber-chat-mode-map)

(setq jabber-alert-presence-message-function (lambda (who oldstatus newstatus statustext) nil))

;; Message alert hooks
;; (define-jabber-alert echo "Show a message in the echo area"
;;   (lambda (msg)
;;     (unless (minibuffer-prompt)
;;       (message "%s" msg))))

(provide 'ruin-hipchat)
