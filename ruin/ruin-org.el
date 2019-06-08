;;; ruin-org.el --- org-mode settings
(package-require 'org)
(package-require 'org-bullets)
(package-require 'helm-org-rifle)
(package-require 'gnuplot)

(require 'org)
(require 'org-bullets)
(require 'ob-ruby)
;(require 'ob-shell)
(require 'ob-gnuplot)
(require 'ob-sql)
(require 'ob-clojure)
(require 'ox-md nil t)

;; Startup & Directories
;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; start agenda on emacs startup
(let ((dir "/mnt/hibiki/up/syncthing/org/"))
  (when (file-exists-p dir)
    (setq org-directory dir)
    (setq org-agenda-files `(,(concat org-directory "tracked/")))
    (setq org-refile-targets `((,(concat org-directory "notes.org") :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9)))

    (setq org-default-notes-file (concat org-directory "tracked/refile.org"))
    (setq org-capture-templates
          `(("t" "todo" entry (file ,(concat org-directory "tracked/refile.org"))
             "* TODO %?\n%U")
            ("c" "todo (with context)" entry (file ,(concat org-directory "tracked/refile.org"))
             "* TODO %?\n%U\n%a")
            ("n" "note" entry (file ,(concat org-directory "notes.org"))
             "* %? :NOTE:\n%U\n")
            ("x" "NeXT task" entry (file+headline ,(concat org-directory "tracked/tasks.org" "NeXT Tasks"))
             "* NEXT %?\nDEADLINE: %t\n%U")
            ("e" "etc." entry (file ,(concat org-directory "notes.org"))
             "* %? - %U\n")
            ("g" "generic" entry (file ,(concat org-directory "tracked/refile.org"))
             "* %?\n%U\n")
            ("d" "diary" entry (file+headline ,(concat org-directory "diary.org" "日記"))
             "* %U\n%?\n" :prepend t)
            ("y" "yume" entry (file+headline ,(concat org-directory "yume.org" "ゆめにっき"))
             "* %U - %? %^g\n" :prepend t)))))

(add-hook 'org-capture-after-finalize-hook 'org-save-all-org-buffers)
(add-hook 'org-agenda-finalize-hook 'org-save-all-org-buffers)

(defun ruin/goto-org-folder ()
  (interactive)
  (find-file org-directory))

;;; Settings
(setq org-src-fontify-natively t
     org-startup-indented t

     org-agenda-sticky nil
     org-agenda-todo-ignore-with-date nil
     org-agenda-todo-ignore-deadlines nil
     org-agenda-todo-ignore-scheduled nil
     org-agenda-todo-ignore-timestamp nil
     org-agenda-compact-blocks t
     org-agenda-window-setup 'current-window
     org-clock-persist 'history
     org-deadline-warning-days 30
     org-capture-empty-lines-after 2
     org-indirect-buffer-display 'current-window
     org-highlight-sparse-tree-matches nil

     org-log-done 'time
     org-log-into-drawer t
     org-log-state-notes-insert-after-drawers nil
     org-return-follows-link t

     org-confirm-babel-evaluate nil
     org-babel-clojure-backend 'cider
     org-babel-clojure-sync-nrepl-timeout nil

     org-hide-emphasis-markers t
     org-pretty-entities t
     org-startup-with-inline-images t
     org-export-with-sub-superscripts nil
     org-use-sub-superscripts nil
     org-startup-folded nil)

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)


(setq org-agenda-time-grid
  '((daily today require-timed)
    "----------------"
    (800 1200 1600 2000)))

(setq org-columns-default-format "%50ITEM(Task) %3PRIORITY %10Effort(Effort){:} %10CLOCKSUM %16TIMESTAMP_IA")
;; (setq org-tags-column 80)
;(setq org-agenda-tags-column org-tags-column)

;;; Agenda Setup

;; bh/helper-functions
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))
(defun bh/find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))
(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

;; Some helper functions for selection within agenda views
(defun gs/select-with-tag-function (select-fun-p)
  (save-restriction
    (widen)
    (let ((next-headline
	   (save-excursion (or (outline-next-heading)
			       (point-max)))))
      (if (funcall select-fun-p) nil next-headline))))

(defun gs/select-projects ()
  "Selects tasks which are project headers"
  (gs/select-with-tag-function #'bh/is-project-p))
(defun gs/select-project-tasks ()
  "Skips tags which belong to projects (and is not a project itself)"
  (gs/select-with-tag-function
   #'(lambda () (and
		 (not (bh/is-project-p))
		 (bh/is-project-subtree-p)))))
(defun gs/select-standalone-tasks ()
  "Skips tags which belong to projects. Is neither a project, nor does it blong to a project"
  (gs/select-with-tag-function
   #'(lambda () (and
		 (not (bh/is-project-p))
		 (not (bh/is-project-subtree-p))))))
(defun gs/select-projects-and-standalone-tasks ()
  "Skips tags which are not projects"
  (gs/select-with-tag-function
   #'(lambda () (or
		 (bh/is-project-p)
		 (bh/is-project-subtree-p)))))

(defun gs/org-agenda-project-warning ()
  "Is a project stuck or waiting. If the project is not stuck,
show nothing. However, if it is stuck and waiting on something,
show this warning instead."
  (if (gs/org-agenda-project-is-stuck)
    (if (gs/org-agenda-project-is-waiting) " !W" " !S") ""))

(defun gs/org-agenda-project-is-stuck ()
  "Is a project stuck"
  (if (bh/is-project-p) ; first, check that it's a project
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
	     (has-next))
	(save-excursion
	  (forward-line 1)
	  (while (and (not has-next)
		      (< (point) subtree-end)
		      (re-search-forward "^\\*+ NEXT " subtree-end t))
	    (unless (member "WAITING" (org-get-tags-at))
	      (setq has-next t))))
	(if has-next nil t)) ; signify that this project is stuck
    nil)) ; if it's not a project, return an empty string

(defun gs/org-agenda-project-is-waiting ()
  "Is a project stuck"
  (if (bh/is-project-p) ; first, check that it's a project
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
	(save-excursion
	  (re-search-forward "^\\*+ WAITING" subtree-end t)))
    nil)) ; if it's not a project, return an empty string

;; Some helper functions for agenda views
(defun gs/org-agenda-prefix-string ()
  "Format"
  (let ((path (org-format-outline-path (org-get-outline-path))) ; "breadcrumb" path
	(stuck (gs/org-agenda-project-warning))) ; warning for stuck projects
       (if (> (length path) 0)
	   (concat stuck ; add stuck warning
		   " [" path "]") ; add "breadcrumb"
	 stuck)))

(defun gs/org-agenda-add-location-string ()
  "Gets the value of the LOCATION property"
  (let ((loc (org-entry-get (point) "LOCATION")))
    (if (> (length loc) 0)
	(concat "{" loc "} ")
      "")))

;; Variables for ignoring tasks with deadlines
(defvar gs/hide-deadline-next-tasks t)
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-deadline-warning-days 10)

(setq org-agenda-custom-commands
      '(("h" "Habits" agenda "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-strategy
           '(todo-state-down effort-up category-keep))))
        (" " "Export Schedule" ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
                                            (org-agenda-span 'day)
                                            (org-agenda-ndays 1)
                                            (org-agenda-start-on-weekday nil)
                                            (org-agenda-start-day "+0d")
                                            (org-agenda-todo-ignore-deadlines nil)))
                                (tags-todo "-INACTIVE-CANCELLED-ARCHIVE/!NEXT"
                                           ((org-agenda-overriding-header "close one door, open the NeXT")
                                            ))
                                (tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVEr/!"
                                           ((org-agenda-overriding-header "Active Projects:")
                                            (org-agenda-skip-function 'gs/select-projects)))
                                (tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE/!-NEXT"
                                           ((org-agenda-overriding-header "Remaining Project Tasks:")
                                            (org-agenda-skip-function 'gs/select-project-tasks)))
                                (tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE-STYLE=\"habit\"/!-NEXT"
                                           ((org-agenda-overriding-header "Standalone Tasks:")
                                            (org-agenda-skip-function 'gs/select-standalone-tasks)))
                                (agenda "" ((org-agenda-overriding-header "Week At A Glance:")
                                            (org-agenda-ndays 5)
                                            (org-agenda-start-day "+1d")
                                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                                            (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %s [%b] ")))))
                                (tags "REFILE-ARCHIVE-REFILE=\"nil\""
                                      ((org-agenda-overriding-header "Tasks to Refile:")
                                       (org-tags-match-list-sublevels nil)))
                                (tags "INACTIVE-ARCHIVE"
                                      ((org-agenda-overriding-header "Inactive Projects and Tasks")
                                       (org-tags-match-list-sublevels nil)))
                                (tags "ENDOFAGENDA"
                                      ((org-agenda-overriding-header "End of Agenda")
                                       (org-tags-match-list-sublevels nil))))
         ((org-agenda-start-with-log-mode t)
          (org-agenda-log-mode-items '(clock))
          (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
                                      (timeline . "  % s")
                                      (todo . "  %-12:c %(gs/org-agenda-prefix-string) ")
                                      (tags . "  %-12:c %(gs/org-agenda-prefix-string) ")
                                      (search . "  %i %-12:c")))
          (org-agenda-todo-ignore-deadlines 'near)
          (org-agenda-todo-ignore-scheduled t)))
        ("X" "Agenda" ((agenda "") (alltodo))
         ((org-agenda-ndays 10)
          (org-agenda-start-on-weekday nil)
          (org-agenda-start-day "-1d")
          (org-agenda-start-with-log-mode t)
          (org-agenda-log-mode-items '(closed clock state)))
	 )))


;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

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



;;; Evil bindings
(package-require 'evil-org)
(require 'evil-org)

(define-key org-mode-map (kbd "<tab>") 'org-cycle)
(define-key org-mode-map (kbd "TAB") 'org-cycle)
(define-key org-mode-map [(tab)] 'org-cycle)
(define-key evil-motion-state-map (kbd "TAB") nil)


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
  "J" 'evil-join
  )

;; leader binds for org-mode
(evil-leader/set-key-for-mode 'org-mode
  "of" 'ruin/goto-org-folder
  "ok" 'org-capture-kill
  "oI" 'org-clock-in
  "oO" 'org-clock-out
  "ol" 'org-store-link
  "oL" 'org-insert-link
  "oid" 'org-deadline
  "oit" 'org-time-stamp-inactive
  "oiT" 'org-time-stamp
  "ois" 'org-schedule
  "oil" 'org-insert-link
  "oic" 'org-insert-code-block
  "ot" 'org-set-tags-command
  "or" 'org-refile
  "ow" 'org-save-all-org-buffers
  "oh" 'helm-org-files
  "oH" 'helm-org-rifle-org-directory)

(package-require 'general)

;; Evil bindings in agenda
(eval-after-load "org-agenda"
  '(progn
     (ruin/window-movement-for-map org-agenda-mode-map)
     (define-key org-agenda-mode-map "j" 'org-agenda-next-item)
     (define-key org-agenda-mode-map "k" 'org-agenda-previous-item)
     ;; Since we override SPC, let's make RET do that functionality
     (define-key org-agenda-mode-map
       (kbd "RET") 'org-agenda-switch-to)
     (define-key org-agenda-mode-map
       (kbd "SPC") evil-leader--default-map)

  (evil-set-initial-state 'org-agenda-mode 'normal)
  (general-define-key
   :keymaps 'org-agenda-mode-map
   :states '(normal motion)
   "l" 'org-agenda-later
   "h" 'org-agenda-earlier
   "j" 'org-agenda-next-line
   "k" 'org-agenda-previous-line
   (kbd "RET") 'org-agenda-switch-to
   [escape] 'org-agenda-quit
   "q" 'org-agenda-quit
   "s" 'org-save-all-org-buffers
   "t" 'org-agenda-todo
   "T" 'org-agenda-set-tags
   "g" 'org-agenda-redo
   "v" 'org-agenda-view-mode-dispatch
   "." 'org-agenda-goto-today
   "J" 'org-agenda-next-item
   "K" 'org-agenda-previous-item
   "c" 'org-agenda-goto-calendar
   "i" 'org-agenda-clock-in
   "o" 'org-agenda-clock-out
   "w" 'org-agenda-refile
   "f" 'org-agenda-follow-mode
   )
  ))

;; enter insert mode on capture
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; automatically narrow on agenda follow
;; http://emacs.stackexchange.com/a/17822
(advice-add 'org-agenda-goto :after
            (lambda (&rest args)
              (org-narrow-to-element)))

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

;; (setq org-use-speed-commands t)

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


;; autosave org buffers
(add-hook 'org-capture-after-finalize-hook #'org-save-all-org-buffers)

(defun org-insert-code-block (language)
  "Asks name, language, switches, header. Inserts org-mode source code snippet"
  (interactive "slanguage? ")
  (insert
   (format "#+BEGIN_SRC %s

#+END_SRC" language))
  (forward-line -1)
  (goto-char (line-end-position))
  (evil-insert-state))

(add-hook 'org-mode-hook (lambda () (yas-minor-mode 0)))

(defun helm-org-files ()
  (interactive)
  (let* ((org-files (directory-files-recursively org-directory "\\`[^\\.].*\\.org\\'"))
         (source (helm-build-sync-source "org-files"
                   :candidates org-files))
         (file (helm :sources source
                     :buffer "*Org Files*"))
         (full-path (expand-file-name (file-name-as-directory org-directory))))
    (when file
      (find-file file))))

(provide 'ruin-org)
