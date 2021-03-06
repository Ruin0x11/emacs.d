;;; ruin-evil.el --- evil settings and non-package mappings

;; Don't move these.
(setq evil-want-C-u-scroll t)
(setq evil-search-module 'evil-search)

(package-require 'evil)
(package-require 'evil-commentary)
(package-require 'evil-surround)
;(load-file (locate-user-emacs-file "site-lisp/evil-leader/evil-leader.el"))
(package-require 'evil-leader)
(package-require 'elisp-refs)
(package-require 'general)
;(package-require 'color-moccur)
;(package-require 'moccur-edit)
(load "evil-leader-minor")
(require 'evil-little-word)
(package-require 'evil-numbers)
(require 'buffer-move)

(global-evil-leader-mode t)
(global-evil-surround-mode t)
(evil-mode 1)
(evil-commentary-mode t)

(add-hook 'after-find-file
          (lambda () (evil-commentary-mode t)))

(setq-default evil-kill-on-visual-paste nil)

;;; leader binds
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "w" 'save-buffer
  "q" 'quit-or-kill-buffer
  "Q" 'kill-buffer-and-window
  "x" 'evil-save-and-close
  "u" 'universal-argument
  "y" 'helm-show-kill-ring
  "!" 'shell-command
  "T" 'crux-visit-term-buffer
  "zz" 'toggle-maximize-buffer
  "ZZZ" 'save-buffers-kill-emacs
  "as" 'sos
  "M" 'popwin:messages
  "<RET>" 'evil-ex-nohighlight

  "df" 'describe-function
  "dv" 'describe-variable
  "dm" 'describe-mode
  "dk" 'describe-key
  "dd" 'describe-foo-at-point
  "de" 'flycheck-list-errors
  "di" 'helm-info

  "eD" 'toggle-debug-on-error
  "eQ" 'toggle-debug-on-quit
  "ee" 'eval-expression
  "ei" 'ielm
  "enn" 'edebug-on-entry
  "enc" 'cancel-edebug-on-entry

  "aa" 'org-agenda
  "c" 'org-capture
  "ob" 'org-iswitchb
  "oc" 'org-clock-goto

  "au" 'undo-tree-visualize
  "ac" 'calc
  "ad" 'diff
  "aw" 'browse-url-at-point
  "ax" 're-builder
  "ap" 'ruin/update-and-list-packages

  "js" 'bookmark-set
  "jj" 'bookmark-jump
  "jd" 'bookmark-delete

  "kc" 'compile
  "kr" 'recompile
  "kk" 'kill-compilation

  "?i" 'helm-info
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

  "TAB" 'spacemacs/alternate-buffer
  "bc"  'ruin/copy-buffer-to-new
  "bd"  'kill-this-buffer
  "bD"  'delete-file-and-buffer
  "bn"  'rename-file-and-buffer
  "bm"  'move-buffer-file
  "br"  'revert-buffer
  "bR"  'modi/revert-all-file-buffers
  "bK"  'spacemacs/kill-other-buffers
  "bY"  'spacemacs/copy-whole-buffer-to-clipboard
  "by"  'ruin/yank-path-of-buffer-file
  "b!"  'shell-command-on-file
  "bB"  'browse-url-of-file
  "b="  'my-diff-buffer-with-file
  "bi"  'indent-buffer
  "ba"  'helm-do-ag-buffers
  "bw"  'whitespace-cleanup
  "bf"  'fit-window-to-buffer)

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

(define-key Info-mode-map (kbd "C-i") 'Info-history-forward)
(define-key Info-mode-map (kbd "C-o") 'Info-history-back)
(define-key Info-mode-map (kbd "C-u") 'Info-scroll-down)
(define-key Info-mode-map (kbd "C-d") 'Info-scroll-up)
(define-key Info-mode-map "m" 'Info-menu)
(ruin/window-movement-for-map Info-mode-map)


(add-hook 'dired-mode-hook
          (lambda()
            (define-key dired-mode-map "f" 'dired-goto-file)
            (define-key dired-mode-map "g" 'dired-goto-file)
            (define-key dired-mode-map (kbd "<DEL>") 'dired-unmark-backward)
            (evil-commentary-mode 0)))


;;; mode-based binds
(defun copy-to-end-of-line ()
  (interactive)
  (lispyville-yank (point) (point-at-eol)))

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

(add-hook 'comint-mode-hook (lambda ()
                              (delete 'comint-mode evil-insert-state-modes)))
(delete 'shell-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'shell-mode)
(add-to-list 'evil-emacs-state-modes 'comint-mode)
(delete 'calc-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'calc-mode)
(ruin/window-movement-for-mode "calc" 'calc-mode-map)

(add-to-list 'evil-emacs-state-modes 'gud-mode)
(add-to-list 'evil-emacs-state-modes 'gud-tooltip-mode)
(add-to-list 'evil-emacs-state-modes 'debugger-mode)
(add-to-list 'evil-emacs-state-modes 'message-mode)

(add-to-list 'evil-emacs-state-modes 'mpc-mode)
(add-to-list 'evil-emacs-state-modes 'mpc-songs-mode)
(add-to-list 'evil-emacs-state-modes 'mpc-status-mode)
(add-to-list 'evil-emacs-state-modes 'mpc-tagbrowser-mode)
(add-to-list 'evil-emacs-state-modes 'mpc-tagbrowser-dir-mode)

(add-to-list 'evil-emacs-state-modes 'inferior-lisp-mode)

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
     ;(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)

     ; (define-key evil-normal-state-map "u"
     ;   (lambda (arg)
     ;     (interactive "P")
     ;     (if arg
     ;         (save-excursion
     ;           (call-interactively 'undo-tree-undo))
     ;       (call-interactively 'undo-tree-undo))))
     ; (define-key evil-normal-state-map (kbd "C-r")
     ;   (lambda (arg)
     ;     (interactive "P")
     ;     (if arg
     ;         (save-excursion
     ;           (call-interactively 'undo-tree-redo))
     ;       (call-interactively 'undo-tree-redo))))

     ;; trade ctrl-h and others for faster window switching
     (ruin/window-movement-for-map evil-normal-state-map)

     (define-key evil-normal-state-map (kbd "M-k") 'buf-move-up)
     (define-key evil-normal-state-map (kbd "M-j") 'buf-move-down)
     (define-key evil-normal-state-map (kbd "M-h") 'buf-move-left)
     (define-key evil-normal-state-map (kbd "M-l") 'buf-move-right)

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

(setq Man-notify-method 'pushy)

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
                                 (define-key mu4e-headers-mode-map "k" 'MU4E-HEADERS-PREV-UNREAD)))

(evil-set-initial-state 'compilation-mode 'normal)
(evil-define-key 'normal compilation-mode-map
  "g?" 'describe-mode
  "gg" 'evil-goto-first-line
  "0" 'evil-digit-argument-or-evil-beginning-of-line
  [mouse-2] 'compile-goto-error
  [follow-link] 'mouse-face
  (kbd "<return>") 'compile-goto-error

  "go" 'compilation-display-error
  (kbd "S-<return>") 'compilation-display-error

  "gj" 'compilation-next-error
  "gk" 'compilation-previous-error
  "[" 'compilation-previous-file
  "]" 'compilation-next-file
  "gr" 'recompile
  "h" 'evil-backward-char
  "?" 'evil-search-backward)

(when (not (or (window-system) (daemonp)))
  (define-key evil-normal-state-map [(tab)]        'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "TAB")    'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "<tab>")  'evil-jump-forward))

;;; normal Emacs bindings
(global-set-key (kbd "C-x C-u") 'universal-argument)
(global-set-key (kbd "C-x C-s") 'sort-lines)
(global-set-key (kbd "C-x |") 'align-regexp)
(global-set-key (kbd "C-x =") 'eval-region)
;; (global-set-key (kbd "M-{") 'xah-insert-brace)

(when (memq system-type '(darwin))
  (global-set-key (kbd "s-n") nil))

(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

(defun ruin/last-error ()
  (interactive)
  (next-error 99999))

(global-set-key [f5] 'ruin/scroll-up-or-prev-buffer)
(global-set-key [f6] 'ruin/scroll-down-or-next-buffer)
(global-set-key [f7] 'previous-error)
(global-set-key [f8] 'next-error)
(global-set-key (kbd "<C-F7>") 'first-error)
(global-set-key (kbd "<C-F8>") 'ruin/last-error)

;(global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)
(global-set-key (kbd "RET") 'indent-new-comment-line)
;(define-key emacs-lisp-mode-map (kbd "RET") 'indent-new-comment-line)

(defun ruin/mark-current-buffer-as-compilation ()
  "Sets the current buffer as the one for compilation next-error."
  (interactive)
  (setq (next-error-last-buffer (window-buffer (selected-window)))))

;; match items with %
(package-require 'evil-matchit)
(global-evil-matchit-mode 1)

(evil-set-initial-state 'compilation-mode 'normal)

(evil-define-key 'normal compilation-mode-map
  "g?" 'describe-mode
  "gg" 'evil-goto-first-line
  "0" 'evil-digit-argument-or-evil-beginning-of-line
  "?" 'evil-search-backward
  [mouse-2] 'compile-goto-error
  [follow-link] 'mouse-face
  (kbd "<return>") 'compile-goto-error

  "go" 'compilation-display-error
  (kbd "S-<return>") 'compilation-display-error

  "gj" 'compilation-next-error
  "gk" 'compilation-previous-error
  "[" 'compilation-previous-file
  "]" 'compilation-next-file
  "gr" 'recompile
  "h" 'evil-backward-char)

(evil-define-key 'normal flycheck-error-list-mode-map
  (kbd "<return>") 'flycheck-error-list-goto-error
  "j" 'flycheck-error-list-next-error
  "k" 'flycheck-error-list-previous-error
  "e" 'flycheck-error-list-explain-error)

(define-key evil-normal-state-map (kbd "<kp-add>") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "<kp-subtract>") 'evil-numbers/dec-at-pt)

;(setq-default evil-search-module 'evil-search)

(defun spacemacs/translate-C-i (_)
  "If `dotspacemacs-distinguish-gui-tab' is non nil, the raw key
sequence does not include <tab> or <kp-tab>, and we are in the
gui, translate to [C-i]. Otherwise, [9] (TAB)."
  (interactive)
  (if (and (not (cl-position 'tab (this-single-command-raw-keys)))
           (not (cl-position 'kp-tab (this-single-command-raw-keys)))
           t
           (display-graphic-p))
      [C-i] [?\C-i]))
(define-key key-translation-map [?\C-i] 'spacemacs/translate-C-i)
(define-key evil-normal-state-map [C-i] 'evil-jump-forward)

(provide 'ruin-evil)
