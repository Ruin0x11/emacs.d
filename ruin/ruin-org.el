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
(let ((dir (concat (getenv "HOME") "/Dropbox/org/")))
  (when (file-exists-p dir)
    (setq org-directory dir
          org-agenda-files `(,(concat org-directory "gtd/"))
          org-refile-targets `((,(concat org-directory "gtd/gtd.org") :maxlevel . 3)
                               (,(concat org-directory "gtd/itsuka.org") :level . 1)
                               (,(concat org-directory "gtd/tickler.org") :maxlevel . 2))

          org-default-notes-file (concat org-directory "gtd/inbox.org")
          ; initial-buffer-choice (concat org-directory "gtd/inbox.org")
          org-capture-templates
          `(("t" "TODO" entry (file ,(concat org-directory "gtd/inbox.org"))
             "* TODO %?\n%U")
            ("T" "Tickler" entry
             (file+headline ,(concat org-directory "gtd/tickler.org") "Tickler")
             "* %i%? \n %U")
            ("j" "Journal" entry
             (file+headline ,(concat org-directory "gtd/journal.org") "Journal")
             "* %(current-time-string) \n%U\n\n*Three things I am grateful for:*\n1. %^{I am grateful for}\n2. %^{I am grateful for}\n3. %^{I am grateful for}\n\n*One positive experience in the last day:*\n%^{One positive experience in the last day}\n\n*What I have learned:*\n1. %^{What I have learned}\n2. %^{What I have learned}\n3. %^{What I have learned}\n\n*What have we done for these facets?*\nHealth:%?\nHappiness:\nRelationships:\nPersonal development:\nFinances:\nCareer:\nWorld-based impact:\n")
            ("R" "Review" entry
             (file+headline ,(concat org-directory "gtd/review.org") "Review")
             "* %(current-time-string) \nStream-of-conscious brief (one paragraph):\n\n%?\n\nProgress towards goals:\n\nNew ideas/risks for personal system:\n\n")
            ("y" "yume" entry (file+headline ,(concat org-directory "yume.org" "ゆめにっき"))
             "* %U - %? %^g\n" :prepend t)))))

(add-hook 'org-capture-after-finalize-hook 'org-save-all-org-buffers)
(add-hook 'org-after-refile-insert-hook 'org-save-all-org-buffers)
(add-hook 'org-agenda-finalize-hook 'org-save-all-org-buffers)
(add-hook 'org-after-todo-state-change-hook 'org-save-all-org-buffers)

(defun ruin/goto-org-folder ()
  (interactive)
  (find-file org-directory))

;;; Settings
(setq org-src-fontify-natively t
     org-startup-indented t
     org-startup-folded 'content

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

     org-ellipsis "⤵")

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(define-key org-mode-map (kbd "M-j") 'org-move-subtree-down)
(define-key org-mode-map (kbd "M-k") 'org-move-subtree-up)

(setq org-agenda-custom-commands
      '(("g" "GTD" tags-todo "*"
         ((org-agenda-overriding-header "プロジェクト")
          (org-agenda-skip-function #'my-org-agenda-skip-projects)))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun my-org-agenda-skip-projects ()
  "Skip projects (items with children)."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (when (or (org-goto-first-child) (not (= (org-current-level) 2)))
        (setq should-skip-entry t)))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))


(setq org-agenda-time-grid
  '((daily today require-timed)
    "----------------"
    (800 1200 1600 2000)))

(setq org-columns-default-format "%50ITEM(Task) %3PRIORITY %10Effort(Effort){:} %10CLOCKSUM %16TIMESTAMP_IA")

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;;; TODOs
(setq org-todo-keywords
      (quote ((sequence "TODO(t!)" "WAITING(w@/!)" "IN PROGRESS(w!)" "CANCELLED(c@/!)" "|" "DONE(d!)"))))

(setq org-todo-keyword-faces
      (quote (
              ("TODO" :foreground "red" :weight bold)
              ;; ("NEXT" :foreground "blue" :weight bold)
              ("IN PROGRESS" :foreground "yellow" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "magneta" :weight bold)
              ("CANCELLED" :foreground "orange" :weight bold)
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
