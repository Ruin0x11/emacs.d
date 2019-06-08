(package-require 'popwin)
(package-require 'multi-term)
(require 'popwin)
;; (require 'open-junk-file)
(popwin-mode)

(defvar ruin/last-popwin-cookie nil
  "The last popwin modeline cookie returned by face-remap-add-relative.")

;; popwin settings
(setq popwin:special-display-config
      '(("*Help*" :height 0.4 :stick t)
        ;; (Man-mode :height 0.4 :stick t)
        ;; ("\\*Man.*" :regexp t :height 0.4 :stick t)
        ;; Debug
        ("*Warnings*" :noselect t :position bottom :height 0.3 )
        ;("*Backtrace*" :stick t :noselect t :position bottom :height 0.3 )
        ("*Messages*" :noselect t :position bottom :height 0.3 )
        ("*Compile-Log*" :position bottom :height 0.3 )
        ("*Shell Command Output*" :position bottom :height 0.3 )
        (".*overtone.log" :regexp t :height 0.3)
        ("project.org.*" :regexp t :position top :noselect t :height 15)
        ("collected.org" :position top :height 15)
        (flycheck-error-list-mode :position bottom :height 0.3 :stick t)
        ("*m4-output*" :position right :stick t :width 80 :noselect t)
        ;; (compilation-mode :position bottom :height 0.3 :noselect t)
        ;; Utils
        ;; ("helm" :regexp t :height 0.3)
        ("*Occur*" :position bottom :height 0.3)
        ("\\*Slime Description.*" :noselect t :regexp t :height 0.3)
        ("*slime-description*" :noselect t :height 0.3)
        ("*sly-description*" :height 0.4 :stick t)
        ("*undo-tree*" :width 0.3 :position right)
        ("*grep*" :position bottom :height 0.2 :stick t :noselect t)
        ("*rg*" :position bottom :height 0.2 :stick t :noselect t)
        ("*xref*" :position bottom :height 0.2 :stick t :noselect t)
        ("*Completions*" :height 0.4)
        ("*compilation*" :height 0.3 :noselect t :stick t)
        ("*quickrun*" :height 0.3 :stick t)
        (projectile-rails-server-mode :height 0.3 :stick t)
        (semantic-symref-results-mode :position right :width 0.3)
        ;(ggtags-global-mode :height 0.3 :stick t)
        ("*quickrun*" :height 0.3 :stick t)
        ;; Magit/vc
        ;; (magit-status-mode :position bottom :noselect t :height 0.3 :stick t)
        ;; ("*magit-commit*" :position bottom :noselect t :height 0.3 :stick t)
        ; ("\\*magit.*" :regexp t :position bottom :noselect t :height 0.3 :stick t)
        ;; ("*magit-diff*" :position bottom :noselect t :height 0.3)
        ;; ("*magit-edit-log*" :position bottom :noselect t :height 0.2)
        ;; ("*magit-process*" :position bottom :noselect t :height 0.2)
        ;; ("*vc-diff*" :position bottom :noselect t :height 0.2)
        ;; ("*vc-change-log*" :position bottom :noselect t :height 0.2)
        ;; Navigator
        ("*Ibuffer*" :position bottom :height 0.2)
        ("*Ido Completions*" :noselect t :height 0.3)
        ("*imenu-tree*" :position left :width 50 :stick t)
        ("*undo-tree*" :stick t :width 0.4 :position right)
        (undo-tree-visualizer-mode :stick t :width 0.4 :position right)
        (direx:direx-mode :position left :width 35 :dedicated t)
        ;(dired-mode :position top)
        ("*gists*" :height 0.3)
        ("*sldb.*":regexp t :height 0.3)
        ("*Gofmt Errors*" :noselect t)
        ("\\*godoc*" :regexp t :height 0.3)
        ("*PowerShell Get-Help*" :noselect t :stick t :height 0.3)
        ;; ("*nrepl-error*" :height 0.2 :stick t)
        ;; ("*nrepl-doc*" :height 0.2 :stick t)
        ;; ("*nrepl-src*" :height 0.2 :stick t)
        ("*Kill Ring*" :height 0.3)
        ("*project-status*" :noselect t)
        ("*Compile-Log*" :height 0.2 :stick t)
        ("*pytest*" :noselect t)
        (rspec-compilation-mode :height 0.4 :tail nil)
        ("\\*Minitest.*\\*" :regexp t :stick t :noselect t :height 0.3 :tail nil)
        ("*projectile-rails-compilation*" :height 0.3 :stick t)
        ("*projectile-rails-generate*" :height 0.3 :stick t)
        ;; Programing
        ("Django:" :regexp t :width 0.3 :position right)
        ("*Python*" :stick t)
        (inf-ruby-mode :stick t :height 0.3)
        ("*lua*" :stick t :height 0.4)
        (inferior-lisp-mode :stick t :height 0.3)
        (minitest-mode :stick t :height 0.3)
        ;; (haskell-interactive-mode :stick t)
        (cargo-process-mode :stick t :noselect t :width 0.25 :position right)
        (racer-help-mode :noselect t :height 0.3)
        ("*jedi:doc*" :noselect t)
        ("*YASnippet tables*" :height 0.4 :stick t)
        ;; (cider-repl-mode :height 0.3 :stick t :noselect t)
        (cider-docview-mode :height 0.4 :stick t :noselect t)
        (cider-stacktrace-mode :height 0.4 :stick t :noselect t)
        (cider-popup-buffer-mode :height 0.3 :stick t)
        ("*cider-result*" :height 0.3)
        ("*cider-result*" :height 0.3)
        (yari-mode :height 0.4 :stick t :dedicated t)
        ("*robe-doc*" :height 0.4 :stick t :dedicated t)
        (sql-interactive-mode :height 0.4 :stick t)
        ;; Console
        ("*shell*" :height 0.3 :stick t)
        (shell-mode :height 0.3 :stick t)
                                        ;("*Async Shell Command*" :height 0.3 :stick t)
        ("\\*ansi-term.*\\*" :regexp t :height 0.3)
        ("\\*terminal.*\\*" :regexp t :height 0.3)
        ("*Bundler*" :height 0.3)
        (term-mode :position :bottom :height 10 :stick t)
        ;; Org/Organized
        ;; (diary-fancy-display-mode :position left :width 50 :stick nil)
        ;; (diary-mode :position bottom :height 15 :stick t)
        ;; (calendar-mode :position bottom :height 15 :stick nil)
        ;; (org-agenda-mode :position bottom :height 15 :stick t)
        ;; ("*Org Agenda.*\\*" :regexp t :position bottom :height 15 :stick t)
        ;; (org-capture-mode :position right :stick t)
        )
      )

(defun ruin/reopen-popwin ()
  "Reopens popwin if possible."
  (interactive)
  (cond (popwin:popup-window
         (popwin:close-popup-window))
        (popwin:popup-last-config
         (popwin:popup-last-buffer))
        (t (popwin-term:multi-term))))

(defun ruin/toggle-popwin ()
  "Opens multi-term if popwin closed, otherwise closes popwin."
  (interactive)
  (if (window-valid-p popwin:popup-window)
      (popwin:close-popup-window)
    (popwin-term:multi-term)))

(defun ruin/mark-current-buffer-as-popwin ()
  "Closes and reopens the selected buffer as the popwin."
  (interactive)
  (when (window-valid-p popwin:popup-window)
    (popwin:close-popup-window))
  (let ((the-buffer (window-buffer (selected-window))))
    (delete-window (selected-window))
    (popwin:popup-buffer the-buffer :height 0.4 :stick t)))

(evil-leader/set-key
  "\'" 'ruin/toggle-popwin
  "\"" 'ruin/reopen-popwin)

;; (defun live-display-ansi ()
;;   (interactive)
;;   (popwin:display-buffer "*ansi-term*"))

;; (defun popwin-term:ansi-term ()
;;   (interactive)
;;   (popwin:display-buffer-1
;;    (or (get-buffer "*ansi-term*")
;;        (save-window-excursion
;;          (interactive)
;;          (ansi-term "/bin/zsh")))
;;    :default-config-keywords '(:position :bottom :height 10 :stick t)))

(defun popwin-term:multi-term ()
  (interactive)
  (popwin:display-buffer-1
   (or (get-buffer "*terminal*")
       (save-window-excursion
         (call-interactively 'multi-term)))
   :default-config-keywords '(:position :bottom :height 10 :stick t)))

(defun close-popwin-if-open ()
  (when popwin:popup-window
      (popwin:close-popup-window)))

;; replace popwin if one is already active
(add-hook 'popwin:before-popup-hook 'close-popwin-if-open)

;; stop eyebrowse from saving to-be-deleted popup window
(add-hook 'eyebrowse-pre-window-switch-hook 'close-popwin-if-open)

;; stop winner from saving popup
(add-hook 'winner-mode-hook 'close-popwin-if-open)

;; (add-hook 'popwin:before-popup-hook
;;           (lambda ()
;;             (when (buffer-live-p ruin/last-popwin-buffer)
;;               (with-current-buffer ruin/last-popwin-buffer
;;                 (when ruin/last-popwin-cookie
;;                   (face-remap-remove-relative ruin/last-popwin-cookie)
;;                   (setq ruin/last-popwin-cookie nil))))
;;             ))

;; (add-hook 'popwin:after-popup-hook
;;           (lambda ()
;;             (setq ruin/last-popwin-buffer popwin:popup-buffer)
;;             (with-current-buffer popwin:popup-buffer
;;               (setq ruin/last-popwin-cookie (face-remap-add-relative
;;                                              'powerline-inactive2 '((:foreground "ivory" :background "DarkOrange2") powerline-inactive2))))))

(provide 'ruin-popwin)
