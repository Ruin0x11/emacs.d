;;;ruin-misc-modes.el --- modes too small for individual .el files

;; semantic
(semantic-mode)

;; winner
(winner-mode)

;; which-key
;; (package-require 'which-key)
;; (setq which-key-idle-delay 0.2)
;; (require 'which-key)
;; (which-key-mode)

;; desktop
(require 'desktop)

;; request
(package-require 'request)

; (desktop-save-mode)

(setq desktop-dirname "~/.emacs.d"
      desktop-base-file-name "desktop"
      desktop-base-lock-name "desktop.lock"
      desktop-save t
      desktop-restore-frames t
      desktop-restore-reuses-frames t
      desktop-restore-in-current-display t
      desktop-restore-forces-onscreen t)

(defun save-desktop ()
  (interactive)
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))

;; savehist
(savehist-mode t)

(setq savehist-file "~/.emacs.d/savehist")

;; quickrun
(package-require 'quickrun)

(setq quickrun-timeout-seconds nil
      quickrun-focus-p nil)

(add-hook 'quickrun-after-run-hook (lambda ()
                                     (quickrun/remove-temp-files)
                                     (quickrun/recenter -5)))
(add-hook 'quickrun/mode-hook      (lambda ()
                                     (quickrun/recenter -5)))

(require 'quickrun)
;; delete active quickrun window and buffer
(defun quickrun/kill-quickrun-buffer ()
  (when (get-buffer quickrun/buffer-name)
    (if (window-live-p (get-buffer-window quickrun/buffer-name))
        (delete-window (get-buffer-window quickrun/buffer-name)))
    (kill-buffer quickrun/buffer-name)))

;; lookup
                                        ; ;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/lookup")
                                        ; (load "lookup-autoloads")
                                        ; (evil-leader/set-key
                                        ;  "ll" 'lookup
                                        ;  "lw" 'lookup-word
                                        ;  "lp" 'lookup-pattern)

                                        ; (load "lookup-autoloads")
                                        ; (setq lookup-mecab-coding-system 'utf-8)
                                        ; (setq lookup-search-agents '(;;(ndmecab)
                                        ;                              (ndict "dict.us.dict.org")
                                        ;                              (ndsary "~/dicts")
                                        ;                              ))
                                        ; (add-to-list 'evil-emacs-state-modes 'lookup-select-mode)
                                        ; (add-to-list 'evil-emacs-state-modes 'lookup-history-mode)
                                        ; (add-to-list 'evil-emacs-state-modes 'lookup-content-mode)
                                        ; (add-to-list 'evil-emacs-state-modes 'lookup-modules-mode)
                                        ; (add-to-list 'evil-emacs-state-modes 'lookup-summary-mode)

;; expand-region
(package-require 'expand-region)
(define-key evil-normal-state-map (kbd "C-'") 'er/expand-region)
(define-key evil-visual-state-map (kbd "C-'") 'er/expand-region)
(define-key evil-normal-state-map (kbd "C-\"") 'er/contract-region)
(define-key evil-visual-state-map (kbd "C-\"") 'er/contract-region)

;; anzu
(package-require 'anzu)
(global-anzu-mode 1)
(setq anzu-cons-mode-line-p nil)

;; aggressive-indent
(package-require 'aggressive-indent)
(add-hook 'emacs-lisp-hook #'aggressive-indent-mode)
(add-hook 'go-mode-hook #'aggressive-indent-mode)
(add-hook 'enh-ruby-mode-hook #'aggressive-indent-mode)

;; arduino-mode
(package-require 'arduino-mode)
(setq auto-mode-alist (remove (rassoc 'arduino-mode auto-mode-alist) auto-mode-alist))

;; processing-mode
(package-require 'processing-mode)
(setq processing-location "/usr/bin/processing-java"
      processing-application-dir "/usr/bin/processing"
      processing-sketchbook-dir "/home/ruin/sketchbook")
(evil-leader/set-key-for-mode 'processing-mode
  "mr" 'processing-sketch-run)

(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))
(add-to-list 'auto-mode-alist '("\\.pde\\'" . processing-mode))

;; Quickrun for processing-mode
(quickrun-add-command "processing"
                      '((:command . "/usr/bin/processing-java")
                        (:exec    . "%c --force --sketch=%d --run --output=%d/output")
                        (:tempfile . nil))
                      :mode 'processing-mode)

;; eyebrowse
(package-require 'eyebrowse)
(eyebrowse-mode t)
(global-set-key (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
(global-set-key (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
(global-set-key (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
(global-set-key (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
(global-set-key (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
(global-set-key (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
(global-set-key (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
(global-set-key (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
(global-set-key (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
(global-set-key (kbd "M-9") 'eyebrowse-switch-to-window-config-9)

;; sos
(require 'sos)

;; crux
(package-require 'crux)
(evil-leader/set-key
  "fw" 'crux-view-url)

;; lively
(require 'lively)
(evil-define-key 'insert emacs-lisp-mode-map (kbd "C-M-l") 'lively)

(package-require 'google-translate)
(setq google-translate-default-target-language "en")
(evil-leader/set-key
  "at" 'google-translate-at-point)

;; markdown-mode
(package-require 'markdown-mode)
(eval-after-load "markdown-mode" #'(lambda ()
                                (define-key markdown-mode-map (kbd "<C-return>") 'markdown-follow-thing-at-point)))

;; cucumber
(package-require 'feature-mode)
(require 'helm-feature)
(evil-leader/set-key-for-mode 'feature-mode
  "tt" 'feature-verify-scenario-at-pos
  "tb" 'feature-verify-all-scenarios-in-buffer
  "ta" 'feature-verify-all-scenarios-in-project
  "th" 'helm-feature-snippets
  )
(add-hook 'compilation-shell-minor-mode-hook
          #'(lambda ()
              (setq compilation-scroll-output nil)))

(setq feature-cucumber-command "bundle exec rake cucumber CUCUMBER_OPTS=\"{options} -r features\" FEATURE=\"{feature}\"")

;; YAML
(package-require 'yaml-mode)

;; diminish
(package-require 'diminish)
(eval-after-load "helm" '(diminish 'helm-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "flycheck" '(diminish 'flycheck-mode))
(eval-after-load "evil-commentary" '(diminish 'evil-commentary-mode))
;; (eval-after-load "paredit" '(diminish 'paredit-mode))
;; (eval-after-load "autopair" '(diminish 'autopair-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))
;; (eval-after-load "highlight-parentheses" '(diminish 'highlight-parentheses-mode))
;; (eval-after-load "subword" '(diminish 'subword-mode))
(eval-after-load "anzu" '(diminish 'anzu-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "magit" '(diminish 'magit-auto-revert-mode))
(eval-after-load "abbrev" '(diminish 'abbrev-mode))
(eval-after-load "evil-smartparens" '(diminish 'evil-smartparens-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "autorevert" '(diminish 'auto-revert-mode))
(eval-after-load "ruby-block" '(diminish 'ruby-block-mode))

(provide 'ruin-misc-modes)
