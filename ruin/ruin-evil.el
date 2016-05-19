;;; ruin-evil.el --- evil settings and non-package mappings

(setq evil-want-C-u-scroll t)
(global-set-key (kbd "C-S-u") 'universal-argument)

(package-require 'evil)
(package-require 'evil-commentary)
(package-require 'evil-surround)
(package-require 'evil-leader)
(evil-mode 1)

(global-evil-leader-mode t)
(global-evil-surround-mode t)
(evil-commentary-mode t)

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
  ":" 'helm-M-x
  "\"" 'crux-visit-term-buffer
  "z" 'zone
  "s" 'sos
  "F" 'flycheck-list-errors
  "M" 'popwin:messages

  "df" 'describe-function
  "dv" 'describe-variable
  "dm" 'describe-mode
  "dk" 'describe-key
  "dd" 'describe-foo-at-point
  "da" 'helm-apropos

  "eD" 'toggle-debug-on-error
  "ee" 'eval-expression
  "ei" 'ielm

  "aa" 'org-agenda
  "c" 'org-capture
  "ob" 'org-iswitchb
  "oc" 'org-clock-goto

  "ac" 'calc
  "ad" 'diff

  "jr" 'helm-register
  "jb" 'helm-bookmarks

  "ff" 'helm-find-files
  "fg" 'helm-do-grep-ag
  "fr" 'helm-recentf
  "fd" 'helm-semantic-or-imenu
  "fs" 'find-function
  "fv" 'find-variable
  "fw" 'download-file-and-open
  "fp" 'find-library
  
  "hR" 'helm-regexp
  "hm" 'helm-man-woman
  "hM" 'helm-mini
  "hM" 'helm-man-woman
  "hr" 'helm-resume
  "hc" 'helm-colors
  "hg" 'helm-google

  "?E" 'info-emacs-manual
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
  "bn"  'rename-file-and-buffer
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


(global-set-key (kbd "C-{") 'winner-undo)
(global-set-key (kbd "C-}") 'winner-redo)
(global-set-key (kbd "C-x M-e") 'eval-and-replace)


(delete 'term-mode evil-insert-state-modes)
(eval-after-load 'evil-vars '(add-to-list 'evil-emacs-state-modes 'term-mode))
;; (evil-set-initial-state 'term-mode 'emacs)
(delete 'shell-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'shell-mode)
(delete 'calc-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'calc-mode)
(ruin/window-movement-for-mode "calc" calc-mode-map)

(add-to-list 'evil-emacs-state-modes 'mpc-mode)
(add-to-list 'evil-emacs-state-modes 'mpc-songs-mode)
(add-to-list 'evil-emacs-state-modes 'mpc-status-mode)
(add-to-list 'evil-emacs-state-modes 'mpc-tagbrowser-mode)
(add-to-list 'evil-emacs-state-modes 'mpc-tagbrowser-dir-mode)

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

(add-hook 'Man-mode-hook '(lambda ()
                            (ruin/window-movement-for-map Man-mode-map)
                            ))

(ruin/window-movement-for-mode "help-mode" 'help-mode-map)
(ruin/window-movement-for-mode "compile" 'compilation-mode-map)

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
