;;; ruin-evil.el --- evil settings and non-package mappings

(setq evil-want-C-u-scroll t)

(package-require 'evil)
(package-require 'evil-commentary)
(package-require 'evil-surround)
;(load-file (locate-user-emacs-file "site-lisp/evil-leader/evil-leader.el"))
(package-require 'evil-leader)
(package-require 'elisp-refs)
(global-evil-leader-mode t)
(load "evil-leader-minor")
(require 'evil-little-word)
(evil-mode 1)

(global-evil-leader-mode t)
(global-evil-surround-mode t)
(evil-commentary-mode t)

;;; leader binds
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "au" 'undo-tree-visualize
  "w" 'save-buffer
  "q" 'quit-or-kill-buffer
  "Q" 'kill-buffer-and-window
  "x" 'evil-save-and-close
  "u" 'universal-argument
  "y" 'helm-show-kill-ring
  "!" 'shell-command
  ":" 'helm-M-x
  "T" 'crux-visit-term-buffer
  "zz" 'toggle-maximize-buffer
  "ZZZ" 'save-buffers-kill-emacs
  "as" 'sos
  "M" 'popwin:messages

  "df" 'describe-function
  "dv" 'describe-variable
  "dm" 'describe-mode
  "dk" 'describe-key
  "dd" 'describe-foo-at-point
  "da" 'helm-apropos

  "eD" 'toggle-debug-on-error
  "eQ" 'toggle-debug-on-quit
  "ee" 'eval-expression
  "ei" 'ielm
  "enn" 'debug-on-entry
  "enc" 'cancel-debug-on-entry

  "aa" 'org-agenda
  "c" 'org-capture
  "ob" 'org-iswitchb
  "oc" 'org-clock-goto

  "ac" 'calc
  "ad" 'diff
  "aw" 'browse-url-at-point
  "al" 'browse-url-generic
  "ax" 're-builder

  "js" 'bookmark-set
  "jj" 'bookmark-jump
  "jd" 'bookmark-delete

  "ff" 'helm-find-files
  "fg" 'helm-do-grep-ag
  "fA" 'helm-do-ag
  "fa" 'helm-do-ag-this-file
  "fr" 'helm-recentf
  "fd" 'helm-semantic-or-imenu
  "fs" 'find-function
  "fv" 'find-variable
  "fw" 'crux-view-url
  "fl" 'find-library
  "fe" 'elisp-refs-function

  "hR" 'helm-regexp
  "hm" 'helm-man-woman
  "hM" 'helm-mini
  "hM" 'helm-man-woman
  "hr" 'helm-resume
  "hc" 'helm-colors
  "hg" 'helm-google

  "kc" 'compile
  "kr" 'recompile
  "kk" 'kill-compilation

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

  "rd" 'delete-frame
  "rn" 'make-frame
  "rr" 'ruin/refactor-name

  "bl" 'helm-buffers-list
  "TAB" 'spacemacs/alternate-buffer
  "bd"  'kill-this-buffer
  "bD"  'delete-file-and-buffer
  "bn"  'rename-file-and-buffer
  "bm"  'move-buffer-file
  "br"  'revert-buffer
  "bK"  'spacemacs/kill-other-buffers
  "bw"  'whitespace-cleanup
  "bY"  'spacemacs/copy-whole-buffer-to-clipboard
  "b!"  'shell-command-on-file
  "bB"  'browse-url-of-file
  "b="  'my-diff-buffer-with-file
  "bi"  'indent-buffer)

(define-key Info-mode-map (kbd "C-i") 'Info-history-forward)
(define-key Info-mode-map (kbd "C-o") 'Info-history-back)
(define-key Info-mode-map "m" 'Info-menu)

(add-hook 'dired-mode-hook
          (lambda()
            (define-key dired-mode-map "f" 'dired-goto-file)
            (define-key dired-mode-map "g" 'dired-goto-file)
            (define-key dired-mode-map (kbd "<DEL>") 'dired-unmark-backward)))


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

(ruin/window-movement-for-map Info-mode-map)

(global-set-key (kbd "C-{") 'winner-undo)
(global-set-key (kbd "C-}") 'winner-redo)
(global-set-key (kbd "C-x M-e") 'eval-and-replace)


(delete 'term-mode evil-insert-state-modes)
(eval-after-load 'evil-vars '(add-to-list 'evil-emacs-state-modes 'term-mode))
(eval-after-load 'evil-vars '(add-to-list 'evil-emacs-state-modes 'inferior-python-mode))
(defadvice term-send-raw (after clear-recorded-key activate)
  (if (string= (kbd "RET") (this-command-keys))
      (clear-this-command-keys)))

(delete 'shell-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'shell-mode)
(delete 'calc-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'calc-mode)
(ruin/window-movement-for-mode "calc" 'calc-mode-map)

(add-to-list 'evil-emacs-state-modes 'gud-mode)
(add-to-list 'evil-emacs-state-modes 'gud-tooltip-mode)
(add-to-list 'evil-emacs-state-modes 'debugger-mode)

(add-to-list 'evil-emacs-state-modes 'mpc-mode)
(add-to-list 'evil-emacs-state-modes 'mpc-songs-mode)
(add-to-list 'evil-emacs-state-modes 'mpc-status-mode)
(add-to-list 'evil-emacs-state-modes 'mpc-tagbrowser-mode)
(add-to-list 'evil-emacs-state-modes 'mpc-tagbrowser-dir-mode)

(general-define-key :states 'emacs "C-h" 'windmove-left)
(general-define-key :states 'emacs "C-j" 'windmove-down)
(general-define-key :states 'emacs "C-k" 'windmove-up)
(general-define-key :states 'emacs "C-l" 'windmove-right)

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
     (define-key evil-normal-state-map "&" 'evil-ex-repeat-substitute-with-flags)

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
     (define-key evil-emacs-state-map [escape] 'evil-normal-state)
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

(eval-after-load "comint" #'(lambda ()
                              (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
                              (define-key comint-mode-map (kbd "<down>") 'comint-next-input)
                        (define-key comint-mode-map (kbd "C-n") 'comint-next-matching-input-from-input)
                        (define-key comint-mode-map (kbd "C-p") 'comint-previous-matching-input-from-input)))


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
                              (define-key mu4e-view-mode-map "u" 'mu4e-update-index)))

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

;;; normal Emacs bindings
(global-set-key (kbd "C-x C-u") 'universal-argument)
(global-set-key (kbd "C-x C-s") 'sort-lines)
(global-set-key (kbd "C-x |") 'align-regexp)
(global-set-key (kbd "C-x =") 'eval-region)
;; (global-set-key (kbd "M-{") 'xah-insert-brace)

(when (memq system-type '(darwin))
  (global-set-key (kbd "s-n") nil))

(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

(global-set-key [f5] 'ruin/scroll-up-or-prev-buffer)
(global-set-key [f6] 'ruin/scroll-down-or-next-buffer)
(global-set-key [f7] 'previous-error)
(global-set-key [f8] 'next-error)
(global-set-key [f9] 'projectile-compile-project)

;; match items with %
(package-require 'evil-matchit)
(global-evil-matchit-mode 1)

(provide 'ruin-evil)
