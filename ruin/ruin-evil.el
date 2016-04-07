;;; ruin-evil.el --- evil settings and non-package mappings

(setq evil-want-C-u-scroll t)
(global-set-key (kbd "C-S-u") 'universal-argument)

(package-require 'evil)
(package-require 'evil-commentary)
(package-require 'evil-surround)
(package-require 'evil-leader)
(package-require 'evil-jumper)
(evil-mode 1)
(evil-jumper-mode 1)

(global-evil-leader-mode)
(global-evil-surround-mode)
(evil-commentary-mode)

;;; leader binds
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "r" (lambda () (interactive) (save-buffer) (quickrun))
  "R" 'quickrun-shell
  "w" 'save-buffer
  "q" 'evil-quit
  "x" 'evil-save-and-close
  "u" 'universal-argument
  "y" 'helm-show-kill-ring
  "!" 'shell-command

  "s" 'sos

  "df" 'describe-function
  "dv" 'describe-variable
  "dm" 'describe-mode
  "dk" 'describe-key
  "dd" 'describe-foo-at-point
  "da" 'helm-apropos

  "eb" 'eval-buffer
  "es" 'eval-last-sexp
  "ee" 'eval-expression
  "eh" 'helm-eval-expression-with-eldoc
  "ed" 'eval-defun
  "eD" 'toggle-debug-on-error

  "aa" 'org-agenda
  "c" 'org-capture
  "ob" 'org-iswitchb
  "oc" 'org-clock-goto

  "ac" 'calc

  "jr" 'helm-register
  "jb" 'helm-bookmarks

  "ff" 'helm-find-files
  "fg" 'helm-do-grep-ag
  "fr" 'helm-recentf
  "fd" 'helm-semantic-or-imenu
  "fs" 'find-function
  "fw" 'download-file-and-open
  
  "hR" 'helm-regexp
  "hM" 'helm-man-woman
  "hm" 'helm-mini
  "hM" 'helm-man-woman
  "hr" 'helm-resume
  "hc" 'helm-colors
  "hg" 'helm-do-grep

  "?e" 'info-emacs-manual
  "?h" 'helm-info-emacs
  ;; "ii" 'helm-info-at-point
  "?l" 'helm-info-elisp
  "?y" 'yas-describe-tables

  "ii" 'other-window
  "i=" 'balance-windows
  "io" 'delete-other-windows
  "ih" 'evil-window-move-far-left
  "il" 'evil-window-move-far-right
  "ij" 'evil-window-move-very-bottom
  "ik" 'evil-window-move-very-top
  "iV" 'split-window-below
  "iS" 'split-window-right
  "iv" 'split-window-below-and-focus
  "is" 'split-window-right-and-focus
  "iu" 'winner-undo
  "iU" 'winner-redo

  "bl" 'helm-buffers-list
  "TAB" 'spacemacs/alternate-buffer
  "bd"  'kill-this-buffer
  "br"  'revert-buffer
  "bD"  'spacemacs/kill-other-buffers
  "bY"  'spacemacs/copy-whole-buffer-to-clipboard
  "b!"  'spacemacs/open-in-external-app
  "b="  'my-diff-buffer-with-file
  )

;;; mode-based binds
(defun copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))

(defun ruin/window-movement-for-mode (mode map)
  (eval-after-load mode `(lambda ()
                   (define-key ,map (kbd "C-h") 'evil-window-left)
                   (define-key ,map (kbd "C-j") 'evil-window-down)
                   (define-key ,map (kbd "C-k") 'evil-window-up)
                   (define-key ,map (kbd "C-l") 'evil-window-right))
            ))

(defun ruin/window-movement-for-map (map)
  (define-key map (kbd "C-h") 'evil-window-left)
  (define-key map (kbd "C-j") 'evil-window-down)
  (define-key map (kbd "C-k") 'evil-window-up)
  (define-key map (kbd "C-l") 'evil-window-right))


;; (global-set-key (kbd "C-h") 'evil-window-left)
;; (global-set-key (kbd "C-j") 'evil-window-down)
;; (global-set-key (kbd "C-k") 'evil-window-up)
;; (global-set-key (kbd "C-l") 'evil-window-right)

(delete 'term-mode evil-insert-state-modes)
(eval-after-load 'evil-vars '(add-to-list 'evil-emacs-state-modes 'term-mode))
;; (evil-set-initial-state 'term-mode 'emacs)
(delete 'shell-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'shell-mode)
(delete 'calc-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'calc-mode)

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(setq sentence-end-double-space nil)

(eval-after-load "evil"
  '(progn
     (define-key evil-normal-state-map "Y" 'copy-to-end-of-line)

     ;; trade ctrl-h and others for faster window switching
     (ruin/window-movement-for-map evil-normal-state-map)

     ;; global escape-to-quit
     (define-key evil-normal-state-map [escape] 'keyboard-quit)
     (define-key evil-visual-state-map [escape] 'keyboard-quit)
     (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
     (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
     (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
     (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
     (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
     (global-set-key [escape] 'evil-exit-emacs-state)
     ))

(add-hook 'term-mode-hook '(lambda ()
                             (ruin/window-movement-for-map term-mode-map)
                             (ruin/window-movement-for-map term-raw-map)
                             (font-lock-mode 1)
                             ))

(add-hook 'man-mode-hook '(lambda ()
                            (ruin/window-movement-for-map man-mode-map)
                             ))

(ruin/window-movement-for-mode "help-mode" 'help-mode-map)

;; j and k where it counts
(eval-after-load "tar" #'(lambda ()
                        (define-key tar-mode-map "j" 'tar-next-line)
                        (define-key tar-mode-map "k" 'tar-previous-line)
                        (define-key tar-mode-map "J" 'scroll-up-next-n-lines)
                        (define-key tar-mode-map "K" 'scroll-down-previous-n-lines)
                        (define-key tar-mode-map ";" 'tar-view)
                        (if (>= emacs-major-version 21)
                            (define-key tar-mode-map "l" 'quit-window)
                          (define-key tar-mode-map "l" 'tar-quit))))

(eval-after-load "mu4e-view" #'(lambda ()
                              (ruin/window-movement-for-map 'mu4e-view-mode-map)
                              (define-key mu4e-view-mode-map (kbd "C-u") 'evil-scroll-up)
                              (define-key mu4e-view-mode-map (kbd "C-d") 'evil-scroll-down)
                              (define-key mu4e-view-mode-map "j" 'evil-next-line)
                              (define-key mu4e-view-mode-map "k" 'evil-previous-line)
                              (define-key mu4e-view-mode-map "g" 'mu4e~headers-jump-to-maildir)
                              (define-key mu4e-view-mode-map "u" 'mu4e-update-index)
                              (define-key mu4e-view-mode-map "J" 'mu4e-view-headers-next-unread)
                              (define-key mu4e-view-mode-map "K" 'mu4e-view-headers-prev-unread)))

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


;;; normal Emacs binds
(global-set-key (kbd "C-x |") 'align-regexp)
(global-set-key (kbd "C-x =") 'eval-region)

(global-set-key [f7] 'previous-error)
(global-set-key [f8] 'next-error)
(global-set-key [f9] 'projectile-compile-project)

(package-require 'evil-matchit)
(global-evil-matchit-mode 1)

(provide 'ruin-evil)
