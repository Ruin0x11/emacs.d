;;; ruin-org.el --- org-mode settings

(package-require 'org)
(package-require 'org-bullets)

(require 'org)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-startup-indented t) 

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/tracked/refile.org")

(setq org-agenda-files (quote ("~/Dropbox/org/tracked")))

(setq org-capture-templates 
      '(("t" "todo" entry (file "~/Dropbox/org/tracked/refile.org")
         "* TODO %?\n%U"
         ;:clock-in t :clock-resume t
         )
        ("c" "todo (with context)" entry (file "~/Dropbox/org/tracked/refile.org")
         "* TODO %?\n%U\n%a"
         ;:clock-in t :clock-resume t
         )
        ("n" "note" entry (file "~/Dropbox/org/tracked/refile.org")
               "* %? :NOTE:\n%U\n")
        ("d" "diary" entry (file+headline "~/Dropbox/org/diary.org" "日記")
         "* %U\n%?\n" :prepend t)
        ("y" "yume" entry (file "~/Dropbox/org/yume.org")
         "* %U - %? %^g\n" :prepend t)
        ))

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)
; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;; Use sticky agendas so they persist
(setq org-agenda-sticky nil)

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

(setq org-deadline-warning-days 30)

(setq org-capture-empty-lines-after 2)

;; Logging
(setq org-log-done (quote time))
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; save at top of hour
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; Evil bindings
(package-require 'evil-org)
(require 'evil-org)
 
(evil-leader/set-key-for-mode 'org-mode
  ;; "o C" 'evil-org-recompute-clocks
  ;; evil-org binds these keys, so we bind them back to their original
  ;; value
  "t" (lookup-key evil-leader--default-map "t")
  "a" (lookup-key evil-leader--default-map "a")
  "b" (lookup-key evil-leader--default-map "b")
  "c" (lookup-key evil-leader--default-map "c")
  "l" (lookup-key evil-leader--default-map "l")
  "o" (lookup-key evil-leader--default-map "o"))

;; Normal mode maps
(evil-define-key 'normal evil-org-mode-map
  (kbd "RET") 'org-open-at-point
  "O" 'evil-open-above)
  

(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))

;; leader binds for org-mode
(evil-leader/set-key-for-mode 'org-mode
  "of" 'org-capture-finalize
  "ok" 'org-capture-kill
  "or" 'org-capture-refile
  "oI" 'org-clock-in
  "oO" 'org-clock-out
  "oid" 'org-deadline
  "oit" 'org-time-stamp-inactive
  "oiT" 'org-time-stamp
  "ois" 'org-schedule
  "oil" 'org-insert-link
  "ot" 'org-set-tags-command
  "oR" 'org-refile
  "os" 'org-save-all-org-buffers
  "oh" 'helm-org-headlines)

;; Evil bindings in agenda
(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
     (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
     (define-key org-agenda-mode-map "T" 'org-agenda-set-tags)
     ;; Since we override SPC, let's make RET do that functionality
     (define-key org-agenda-mode-map
       (kbd "RET") 'org-agenda-switch-to)
     (define-key org-agenda-mode-map
       (kbd "SPC") evil-leader--default-map)))

;; enter insert mode on capture
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; custom helm completion handler for refiling
(when (and (boundp 'org-completion-handler)
           (require 'helm nil t))
  (defun org-helm-completion-handler
      (prompt collection &optional predicate require-match
              initial-input hist def inherit-input-method)
    (helm-comp-read prompt
                    collection
                    ;; the character \ is filtered out by default ;(
                    :fc-transformer nil
                    :test predicate
                    :must-match require-match
                    :initial-input initial-input
                    :history hist
                    :default def))
  
  (setq org-completion-handler 'org-helm-completion-handler))

(setq org-use-speed-commands t)

;; speed up big files
(defun disable-linum-mode-in-big-files ()
  (if (> (count-words (point-min) (point-max)) 10000)
      (linum-mode -1)))

(add-hook 'org-mode-hook 'disable-linum-mode-in-big-files)

;; improve performance on large org files by tweaking linum
(setq linum-delay t 
      linum-eager nil)

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; The following custom-set-faces create the highlights
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t))

(provide 'ruin-org)
