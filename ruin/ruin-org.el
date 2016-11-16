;;; ruin-org.el --- org-mode settings
(package-require 'org)
(package-require 'org-bullets)

(setq org-agenda-files (quote ("~/Dropbox/org/tracked")))

(require 'org)
(require 'org-bullets)

(require 'ob-ruby)
(require 'ob-R)
(require 'ob-sh)

;; Startup & Directories
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-startup-indented t)

(setq org-default-notes-file "~/Dropbox/org/tracked/refile.org")
           

(setq org-default-notes-file "~/Dropbox/org/tracked/refile.org")
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
        ("i" "class" entry (file "~/Dropbox/org/school.org")
               "* %U\n%?\n")
        ("e" "etc." entry (file "~/Dropbox/org/notes.org")
               "* %? - %U\n")
        ("d" "diary" entry (file+headline "~/Dropbox/org/diary.org" "日記")
        "* %U\n%?\n" :prepend t)
        ("y" "yume" entry (file+headline "~/Dropbox/org/yume.org" "ゆめにっき")
         "* %U - %? %^g\n" :prepend t)
        )) 
;; save at top of hour
;; (run-at-time "00:59" 3600 'org-save-all-org-buffers)


;;; Clocking
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


;;; Agenda & Capture
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

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

(setq org-highlight-sparse-tree-matches nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

(setq org-agenda-custom-commands
      (quote (("n" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("d" "Timeline for today"
               ((agenda "" )

                ))
              ("a" "Agenda EX"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                )))))


;;; TODOs
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (
              ;; ("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ;; ("DONE" :foreground "forest green" :weight bold)
              ;; ("WAITING" :foreground "magneta" :weight bold)
              ;; ("CANCELLED" :foreground "orange" :weight bold)
              )))


;;; Logging
(setq org-log-done (quote time))
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)


;;; Refile
                                        ; Exclude DONE state tasks from refile targets
;; (defun bh/verify-refile-target ()
;;   "Exclude todo keywords with a done state from refile targets"
;;   (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; (setq org-refile-target-verify-function 'bh/verify-refile-target)

                                        ; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;;; Babel
(setq org-babel-confirm-evaluate nil)

;;; Other
(setq org-hide-emphasis-markers t)

;;; Evil bindings
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
  "O" 'evil-open-above
  ;; "P" 'bh/narrow-to-project
  ;; "Q" 'bh/widen
  ;; "U" 'bh/narrow-up-one-level
  ;; "S" 'bh/narrow-to-subtree
  ;; avoid conflict with C-i and Tab
  ;; https://github.com/bling/evil-jumper/issues/8
  ;; [tab] 'org-cycle)
  )

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
  "ow" 'org-save-all-org-buffers
  "oh" 'helm-org-agenda-files-headings)

;; Evil bindings in agenda
(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
     (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
     ;; Since we override SPC, let's make RET do that functionality
     (define-key org-agenda-mode-map
       (kbd "RET") 'org-agenda-switch-to)
     (define-key org-agenda-mode-map
       (kbd "SPC") evil-leader--default-map)))

;; Norang project narrowing in agenda
(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "T" 'bh/org-todo))
          'append)

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "Q" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
          'append)

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
          'append)

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "S" 'bh/narrow-to-subtree))
          'append)

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

;; enter insert mode on capture
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; automatically narrow on agenda follow
;; http://emacs.stackexchange.com/a/17822
(advice-add 'org-agenda-goto :after
            (lambda (&rest args)
              (org-narrow-to-element)))

;;; Encryption
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

;;; Misc.
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

;; autosave after capture / TODO state change
(add-hook 'org-capture-after-finalize-hook
          '(lambda ()
             (org-save-all-org-buffers)
             (my-org-agenda-to-appt)))
(add-hook 'org-after-todo-state-change-hook
          '(lambda ()
             (org-save-all-org-buffers)
             (my-org-agenda-to-appt)))

;; give visual notifications of deadlines with appt
;; http://emacs.stackexchange.com/a/5821
(require 'appt)
(appt-activate t)

(setq appt-message-warning-time 30)
(setq appt-display-interval 10) ; disable multiple reminders
(setq appt-display-mode-line t)

                                        ; use appointment data from org-mode
(defun my-org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

                                        ; run when starting Emacs and everyday at 12:05am
(my-org-agenda-to-appt)
(run-at-time "12:05am" (* 24 3600) 'my-org-agenda-to-appt)

                                        ; display appointments as a notifications in the window manager
(setq appt-disp-window-function 'my-appt-display)

(setq my-appt-notification-app (concat (getenv "HOME") "/.bin/appt-notification"))

(defun my-appt-display (min-to-app new-time msg)
  (if (atom min-to-app)
      (call-process my-appt-notification-app nil nil nil min-to-app msg)
    (dolist (i (number-sequence 0 (1- (length min-to-app))))
      (call-process my-appt-notification-app nil nil nil (nth i min-to-app) (nth i msg)))))

                                        ; automatically update appointments when TODO.txt is saved
(add-hook 'after-save-hook
          '(lambda ()
             (if (string= (buffer-file-name) (concat (getenv "HOME") "/ideas/TODO.txt"))
                 (my-org-agenda-to-appt))))

;; start agenda on emacs startup
;; (add-hook 'emacs-startup-hook
;;           '(lambda ()
;;              (setq org-agenda-files (quote ("~/org/tracked")))
;;              (org-agenda nil "a")
;;              (previous-window)))

(provide 'ruin-org)
