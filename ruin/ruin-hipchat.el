;; ruin-hipchat.el
(package-require 'jabber)
(require 'jabber)

(setq jabber-history-enabled t
      jabber-use-global-history nil
      jabber-backlog-number 40
      jabber-backlog-days 30)

(evil-leader/set-key
 "jc" 'jabber-connect
 "jd" 'jabber-disconnect
 "jj" 'hipchat-join
 "jw" 'jabber-chat-with)

(add-hook 'jabber-alert-message-hooks 'jabber-message-libnotify) 

(setq jabber-alert-presence-message-function (lambda (who oldstatus newstatus statustext) nil))

;; Message alert hooks
;; (define-jabber-alert echo "Show a message in the echo area"
;;   (lambda (msg)
;;     (unless (minibuffer-prompt)
;;       (message "%s" msg))))

(provide 'ruin-hipchat)
