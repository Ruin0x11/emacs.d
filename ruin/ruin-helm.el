;;; ruin-helm.el --- helm settings
(package-require 'helm)
(package-require 'helm-ag)
(package-require 'helm-swoop)
(package-require 'helm-flx)
(require 'helm-config)
(eval-after-load "helm-net" '(require 'helm-google))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-quick-update t
      helm-bookmark-show-location t
      helm-buffers-fuzzy-matching t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t
      helm-candidate-number-limit 50
      helm-input-idle-delay 0.1
      )

(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode t)
(helm-flx-mode +1)

(evil-leader/set-key
  "/"  'helm-swoop

  "ff" 'helm-find-files
  "fg" 'helm-do-grep-ag
  "fr" 'helm-recentf
  "fd" 'helm-semantic-or-imenu
  "fF" 'helm-find

  "hf" 'helm-flycheck
  "hR" 'helm-regexp
  "hm" 'helm-man-woman
  "hM" 'helm-mini
  "hb" 'helm-bookmarks
  "hr" 'helm-resume
  "hc" 'helm-colors
  "hg" 'helm-google

  ;; "ii" 'helm-info-at-point
  "?e" 'helm-info-emacs
  "?l" 'helm-info-elisp
  "?c" 'helm-info-calc)


(defcustom browse-url-surf-arguments nil
  "A list of strings to pass to surf as arguments."
  :type '(repeat (string :tag "Argument"))
  :version "24.1"
  :group 'browse-url)

;;;###autoload
(defun browse-url-surf (url &optional _new-window)
  "Ask the surf WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-surf-arguments' are also passed to
surf."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
	   (concat "surf " url) nil
           "surf"
	   (append
	    browse-url-surf-arguments
	    (list url)))))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(defadvice helm-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun prev-window ()
  (interactive)
  (other-window -1))

(provide 'ruin-helm)
