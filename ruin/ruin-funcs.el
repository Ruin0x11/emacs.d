;;; ruin-funcs.el --- various function definitions

;;; Ruin functions

(defun ruin/haskell-interactive-switch-and-move ()
  (interactive)
  (haskell-interactive-switch)
  (goto-char (point-max))
  (evil-append 1))

(defun ruin/haskell-load-file-and-switch ()
  (interactive)
  (haskell-process-load-file)
  (ruin/haskell-interactive-switch-and-move))

(defun ruin/write-and-eval-buffer ()
  (interactive)
  (save-buffer)
  (eval-buffer))


(defun ruin/write-and-eval-defun ()
  (interactive)
  (save-buffer)
  (eval-defun))

(defun ruin/set-shift-width-for-mode (mode-hook width)
  (add-hook mode-hook
            `(lambda ()
               (setq evil-shift-width ,width))))

(defun ruin/refactor-name (&optional newsym)
  "Refactors the name at point in the current buffer unconditionally."
  (interactive)
  (let* ((sym (symbol-name (symbol-at-point)))
         (newsym (or newsym
                     (read-string (concat "Replace \"" sym "\" with: "))))
         (regexp (concat "\\_<\\(" (regexp-quote sym) "\\)\\_>")))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (while (re-search-forward regexp nil t)
          (replace-match newsym t nil))))))

(defun ruin/evil-block-size ()
  "Gives the size of the block area delimited by an evil '%' at point.

   Useful for seeing the size of large functions."
  (interactive)
  (let ((current-line (line-number-at-pos)))
    (save-excursion
      (evil-jump-item)
      (message (int-to-string
                (abs (- current-line (line-number-at-pos))))))))

(defun ruin/scroll-down-or-next-buffer ()
  (interactive)
  (let ((count (/ (1- (window-height)) 2)))
    (if (pos-visible-in-window-p (point-max))
        (progn
          (next-buffer)
          (goto-char (point-min)))
      (scroll-up count))))

(defun ruin/scroll-up-or-prev-buffer ()
  (interactive)
  (let ((count (/ (1- (window-height)) 2)))
    (if (pos-visible-in-window-p (point-min))
        (progn
          (previous-buffer)
          (goto-char (point-max)))
      (scroll-down count))))

(defun ruin/async-shell-command-no-output (command)
  "Run COMMAND asynchronously without opening the output buffer."
  (let ((display-buffer-alist
         (cons
          (cons "\\*Async Shell Command\\*.*"
                (cons #'display-buffer-no-window nil))
          display-buffer-alist)))
    (async-shell-command command nil nil)))

(defun ruin/load-encrypted (filename)
  "Load the encrypted Emacs Lisp file FILENAME."
  (let ((temp-file (make-temp-file "epa"))
        (filename (expand-file-name filename)))
    (epa-decrypt-file filename temp-file)
    (load filename nil nil)))

(defun ruin/advice-clear (sym)
  (interactive "aFunction? ")
  (advice-mapc (lambda (p a) (advice-remove sym p)) sym))

;;; General functions
(defun directory-files-exclude (directory &optional full match nosort)
  "Like `directory-files', but excluding \".\" and \"..\"."
  (delete "." (delete ".." (directory-files directory full match nosort))))

(defun ruin/insert-template (file)
 "Insert a template file from the \"misc\" directory."
  (interactive
   (list
    (completing-read "Choose one: " (directory-files-exclude (locate-user-emacs-file "misc")))))
  (let ((template-dir (locate-user-emacs-file "misc")))
    (insert-file-contents (concat template-dir "/" file))))

(defun ruin/symbol-usage-count ()
  (interactive)
  (let* ((sym (symbol-name (symbol-at-point)))
         (regexp (concat "\"" (regexp-quote sym) "\""))
         (count (shell-command-to-string (concat "rg --stats -q " regexp " " (projectile-project-root) " | sed -n '2p'"))))
    (beginning-of-line)
    (insert (concat "// " count))
    (join-line)
    (beginning-of-line)
    (next-line)
    ))

(require 'url)

(defun download-file-and-open (&optional url download-dir download-name)
  (interactive)
  (let* ((url (or url
                  (read-string "Enter download URL: ")))
         (file-name (or download-name
                        (car (last (split-string url "/" t)))))
         (file-dir (concat (or download-dir
                               "~/ダウンロード/")
                           file-name)))
    (if (not (file-exists-p file-dir))
        (let ((download-buffer (url-retrieve-synchronously url)))
          ;; (save-excursion
          (set-buffer download-buffer)
          ;; we may have to trim the http response
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (forward-char)
          (delete-region (point-min) (point))
          (write-file file-dir)
          (find-file file-dir))
      (write-file file-dir t)
      (find-file file-dir))))
;; )

(defun quit-or-kill-buffer ()
  (interactive)
  (if (= (count-windows) 1)
      (kill-this-buffer)
    (evil-quit)))

(defun shell-command-on-buffer ()
  "Asks for a command and executes it in inferior shell with current buffer
as input."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (read-shell-command "Shell command on buffer: ")))

(defun shell-command-on-file (command)
  "run a command on the current file and revert the buffer"
  (interactive "sCommand: ")
  (async-shell-command
   (format (concat command " %s")
           (shell-quote-argument (buffer-file-name)))))

;;; from elsewhere

;;http://www.emacswiki.org/emacs/DescribeThingAtPoint#toc2
;;; describe this point lisp only
(defun describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.
This checks in turn:
-- for a function name where point is
-- for a variable name where point is
-- for a surrounding function call
"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
                           (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (describe-function sym))
          ((setq sym (variable-at-point)) (describe-variable sym))
          ;; now let it operate fully -- i.e. also check the
          ;; surrounding sexp for a function call.
          ((setq sym (function-at-point)) (describe-function sym)))))


;;http://emacs.stackexchange.com/q/3776
(defun my-diff-buffer-with-file ()
  "Compare the current modified buffer with the saved version."
  (interactive)
  (recover-this-file)
  (let ((diff-switches "-u")) ;; unified diff
    (diff-buffer-with-file (current-buffer))))

;;http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


;;https://news.ycombinator.com/item?id=11488417
(defconst trc-comment-keywords "\\<\\(FIXME\\|TODO\\|BUG\\|HACK\\|NOTE\\|WARNING\\|ERROR\\|IMPLEMENT\\|TEMP\\)")

;; Install the word coloring
(defun add-comment-keywords ()
  (font-lock-add-keywords nil
                          `((,trc-comment-keywords 1 font-lock-warning-face t))))

(add-hook 'find-file-hooks 'add-comment-keywords t)
(set-face-underline 'font-lock-warning-face "yellow") ; Just make sure we'll see it

(defun list-comment-notes ()
  "List all TODO/FIXME/HACK, itp. in a new buffer for reference."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((collected-lines '()))
      (while (re-search-forward trc-comment-keywords nil t)
        ;; collect lines
        (setq collected-lines (cons
                               (format "%d: %s" (line-number-at-pos) (thing-at-point 'line t))
                               collected-lines)))

      ;; generate a new buffer
      (let ((notes-buffer (generate-new-buffer (concat (buffer-name) "-comment-notes"))))
        (set-buffer notes-buffer)
        ;; dump collected stuff to here.
        (dolist (a-line collected-lines)
          (insert a-line)
          (insert "\n"))))))

(defun re-seq-lines (regexp string)
  "Get a list of all lines matching regexes in a string"
  (save-match-data
    (let ((case-fold-search nil)
          (pos 0)
          matches)
      (while (string-match regexp string pos)
        (goto-char (match-beginning 0))
        (push (thing-at-point 'line) matches)
        (setq pos (match-end 0)))
      matches)))

                                        ;http://emacs.stackexchange.com/a/7150
(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))
; Sample URL
;(setq urlreg "\\(?:http://\\)?www\\(?:[./#\+-]\\w*\\)+")
; Sample invocation
;(re-seq urlreg (buffer-string))

(defun save-defaults ()
  (desktop-save desktop-dirname)
  (savehist-save)
  (bookmark-save))

;; (defun save-histories ()
;;   (let ((buf (current-buffer)))
;;     (save-excursion
;;       (dolist (b (buffer-list))
;;         (switch-to-buffer b)
;;         (save-history)))
;;     (switch-to-buffer buf)))

(defun save ()
  (interactive)
  (recentf-save-list)
  (save-desktop)
  (save-defaults)
  ;; (save-histories)
  )

;;http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (when (get-buffer new-name)
          (kill-buffer new-name))
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil)
             t))))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (if (yes-or-no-p "Delete this file? ")
      (let ((filename (buffer-file-name)))
        (when filename
          (if (vc-backend filename)
              (vc-delete-file filename)
            (progn
              (delete-file filename)
              (message "Deleted file %s" filename)
              (kill-buffer)))))))

(defun ruin/copy-buffer-to-new ()
  (interactive)
  (let ((newname (concat (buffer-name) "-copy")))
    (get-buffer-create newname)
    (copy-to-buffer newname (point-min) (point-max))
    (switch-to-buffer newname)
    (message (concat  "Copied contents to " newname))))

;;https://www.emacswiki.org/emacs/TransparentEmacs
;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

;; http://whattheemacsd.com/buffer-defuns.el-03.html
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; https://stackoverflow.com/a/18034042
(define-key process-menu-mode-map (kbd "k") 'joaot/delete-process-at-point)

(defun joaot/delete-process-at-point ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (cond ((and process
                (processp process))
           (delete-process process)
           (revert-buffer))
          (t
           (error "no process at point!")))))

;; from scimax
;;;###autoload
(defun explorer ()
  "Open Finder or Windows Explorer in the current directory."
  (interactive)
  (cond
   ((string= system-type "darwin")
    (shell-command (format "open -b com.apple.finder %s"
			   (if (buffer-file-name)
			       (file-name-directory (buffer-file-name))
			     "~/"))))
   ((string= system-type "windows-nt")
    (shell-command (format "explorer %s"
			   (replace-regexp-in-string
			    "/" "\\\\"
			    (if (buffer-file-name)
				(file-name-directory (buffer-file-name))
			      (expand-file-name  "~/"))))))))

(defalias 'finder 'explorer "Alias for `explorer'.")


;;; Bodil

;;https://github.com/bodil/emacs.d/blob/master/bodil/bodil-defuns.el#L17
(defun font-lock-replace-symbol (mode reg sym)
  (font-lock-add-keywords
   mode `((,reg
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ,sym 'decompose-region)))))))
(defun add-hooks (modes func)
  (dolist (mode modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) func)))

(defun strip-text-properties(txt)
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (strip-text-properties (match-string 0 string)) matches)
        (setq pos (match-end 0)))
      matches)))

(defconst urlreg "\\(?:https?://\\)\\([A-Za-z]+\\)\\(?:[./#\+-]\\(\\w\\|[&;=_?]\\)*\\)+"
)

(defun url-list ()
  (mapconcat 'identity
             (re-seq urlreg (buffer-string))
             "\n"))

(defun yank-buffer-url-list ()
  "Copy all URLs in the current buffer to the kill ring."
  (interactive)
  (kill-new (url-list))
  (message "Copied URLs."))

;; Crux
;;https://github.com/bbatsov/crux/blob/master/crux.el#L258
(defun crux-view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point-min) (point))
    (delete-blank-lines)
    (buffer-enable-undo)
    (set-auto-mode)))

(defvar crux-term-buffer-name "multi"
  "The default `ansi-term' name used by `crux-visit-term-buffer'.
This variable can be set via .dir-locals.el to provide multi-term support.")

(defun crux-start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Do clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer buffer-name)))

(defun crux-visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (crux-start-or-switch-to (lambda ()
                             (ansi-term "/bin/zsh" (concat crux-term-buffer-name "-term")))
                           (format "*%s-term*" crux-term-buffer-name)))


;; Spacemacs
(defun spacemacs/alternate-buffer ()
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (if (evil-alternate-buffer)
      (switch-to-buffer (car (evil-alternate-buffer)))
    (switch-to-buffer (other-buffer (current-buffer) t))))

(defun spacemacs/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (let (name (buffer-name))
    (when (yes-or-no-p (format "Killing all buffers except \"%s\" ? " buffer-file-name))
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
      (message "Buffers deleted!"))))

;; http://stackoverflow.com/a/10216338/4869
(defun spacemacs/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun spacemacs/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun spacemacs/system-is-mac ()
  (string-equal system-type "darwin"))
(defun spacemacs/system-is-linux ()
  (string-equal system-type "gnu/linux"))
(defun spacemacs/system-is-mswindows ()
  (string-equal system-type "windows-nt"))

(defun spacemacs/open-in-external-app ()
  "Open current file in external application."
  (interactive)
  (let ((file-path (if (eq major-mode 'dired-mode)
                       (dired-get-file-for-visit)
                     (buffer-file-name))))
    (if file-path
        (cond
         ((spacemacs/system-is-mswindows) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
         ((spacemacs/system-is-mac) (shell-command (format "open \"%s\"" file-path)))
         ((spacemacs/system-is-linux) (let ((process-connection-type nil))
                                        (start-process "" nil "xdg-open" file-path))))
      (message "No file associated to this buffer."))))

(defun spacemacs/recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch
Emacs versions."
  (interactive)
  (byte-recompile-directory package-user-dir nil t))


(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))
(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

;; https://github.com/magnars/.emacs.d/blob/master/defuns/lisp-defuns.el#L3
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; https://www.reddit.com/r/emacs/comments/4531i9/how_to_efficiently_insert_quotes_parens_or/
(defun xah-insert-bracket-pair (φleft-bracket φright-bracket)
  "Wrap or Insert a matching bracket and place cursor in between.

If there's a text selection, wrap brackets around it. Else, smartly decide wrap or insert. (basically, if there's no char after cursor, just insert bracket pair.)

φleft-bracket ＆ φright-bracket are strings.

URL `http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html'
Version 2015-04-19"
  (if (use-region-p)
      (progn
        (let (
              (ξp1 (region-beginning))
              (ξp2 (region-end)))
          (goto-char ξp2)
          (insert φright-bracket)
          (goto-char ξp1)
          (insert φleft-bracket)
          (goto-char (+ ξp2 2))
          (indent-region ξp1 ξp2)))
    (progn ; no text selection
      (if
          (or
           (looking-at "[^-_[:alnum:]]")
           (eq (point) (point-max)))
          (progn
            (insert φleft-bracket φright-bracket)
            (search-backward φright-bracket ))
        (progn
          (let (ξp1 ξp2)
            ;; basically, want all alphanumeric, plus hyphen and underscore, but don't want space or punctuations. Also want chinese.
            ;; 我有一帘幽梦，不知与谁能共。多少秘密在其中，欲诉无人能懂。
            (skip-chars-backward "-_[:alnum:]")
            (setq ξp1 (point))
            (skip-chars-forward "-_[:alnum:]")
            (setq ξp2 (point))
            (goto-char ξp2)
            (insert φright-bracket)
            (goto-char ξp1)
            (insert φleft-bracket)
            (goto-char (+ ξp2 (length φleft-bracket)))))))))
(defun xah-insert-brace () (interactive) (xah-insert-bracket-pair "{\n" "\n}") )

;; Norang
(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))
          (org-agenda-redo))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
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

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
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
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ;; ((org-is-habit-p)
       ;;  next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

;;; Project Narrowing

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(defun bh/widen ()
  (interactive)
  (org-agenda-remove-restriction-lock)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (bh/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (bh/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (bh/narrow-to-org-subtree)))
(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (org-get-at-bol 'org-hd-marker)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-up-one-org-level))
        (org-agenda-redo))
    (bh/narrow-up-one-org-level)))

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-to-org-project)
          (save-excursion
            (bh/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (beginning-of-buffer))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(provide 'ruin-funcs)
