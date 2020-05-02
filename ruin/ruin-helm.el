;; -*- lexical-binding: t -*-
;;; ruin-helm.el --- helm settings
(package-require 'helm)
(package-require 'helm-ag)
(package-require 'helm-swoop)
(package-require 'helm-flx)
(package-require 'wgrep)
(package-require 'rg)
(package-require 'ivy)
(package-require 'counsel)
(package-require 'smex)
(package-require 'ivy-hydra)

(ivy-mode t)

(require 'helm-config)
(require 'wgrep)
;(require 'helm-fd)

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
      helm-ag-base-command "ag --vimgrep --nocolor "
      helm-fd-command-option "-H"
      helm-input-idle-delay 0.05
      helm-cycle-resume-delay 2
      helm-follow-input-idle-delay 0.2
      helm-M-x-always-save-history t)

(with-eval-after-load 'rg
  (push '("hcl" . "*.hcl") rg-custom-type-aliases)
  (push '("slim" . "*.slim") rg-custom-type-aliases)
  (push '("hsp" . "*.hsp") rg-custom-type-aliases)
  (push '("yml" . "*.yml") rg-custom-type-aliases)
  (push '("hpp" . "*.hpp") rg-custom-type-aliases)
  (push '("janet" . "*.janet") rg-custom-type-aliases)
)

;; dumb redefinition for ripgrep
;; (defun helm-projectile-ag (&optional options)
;;   "Helm version of projectile-ag."
;;   (interactive (if current-prefix-arg (list (read-string "option: " "" 'helm-ag--extra-options-history))))
;;   (if (require 'helm-ag nil  'noerror)
;;       (if (projectile-project-p)
;;           (let ((helm-ag-command-option options)
;;                 (current-prefix-arg nil))
;;             (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
;;         (error "You're not in a project"))
;;     (error "helm-ag not available")))

(global-set-key (kbd "M-x") 'counsel-M-x)
; (helm-mode t)
; (helm-flx-mode +1)
(ivy-mode t)
(helm-mode 0)

(define-key ivy-minibuffer-map (kbd "C-z") 'hydra-ivy/body)
(define-key ivy-minibuffer-map (kbd "TAB") 'hydra-ivy/body)

(rg-define-search ruin/rg-current-file
  "Search for thing in files matching the current file name (as a
pattern) under the current directory."
  :files (file-name-nondirectory (buffer-file-name))
  :dir current)

(evil-leader/set-key
  "/"  'swiper
  ":" 'counsel-M-x

  "ff" 'helm-find-files
  "fa" 'counsel-rg
  "fA" 'ruin/rg-current-file
  "fr" 'counsel-recentf
  "fd" 'counsel-semantic-or-imenu
  "fi" 'counsel-imenu
  ; "fI" 'helm-fd

  "fs" 'find-function
  "fv" 'find-variable
  "fl" 'find-library
  "fe" 'elisp-refs-function
  "fO" 'dmoccur

  "hm" 'man
  "hr" 'helm-resume
  "hR" 'ivy-resume
  "hc" 'counsel-colors-web

  "hm" 'helm-man-woman
  "hM" 'helm-mini
  "hM" 'helm-man-woman
  "hR" 'helm-resume
  "hr" 'ivy-resume
  "hc" 'helm-colors
  "hg" 'helm-google
  "hf" 'counsel-recoll

  "da" 'counsel-apropos

  "bl" 'counsel-switch-buffer
  "bL" 'popwin:pop-to-buffer

  ;; "ii" 'helm-info-at-point
  "?e" 'helm-info-emacs
  "?l" 'helm-info-elisp
  "?g" 'helm-info-magit
  "?c" 'helm-info-calc
  "?o" 'helm-info-org)

(define-key ivy-mode-map (kbd "M-k") 'ivy-kill-ring-save)

(require 'pulse)

(advice-add 'dumb-jump-result-follow :after
            (lambda (&rest args)
              (pulse-momentary-highlight-one-line (point))))

(setq browse-url-text-browser "links"
      browse-url-firefox-program "firefox-developer-edition")

(case system-type
  (gnu/linux (setq browse-url-browser-function 'browse-url-firefox))
  (darwin (progn
            (setq browse-url-browser-function 'browse-url-generic)
            (setq browse-url-generic-program "open"))))


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


(defun prev-window ()
  (interactive)
  (other-window -1))

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; please just do zsh completion
(with-eval-after-load 'helm-files
  ;; Let helm support zsh-like path expansion.
  (defvar helm-ff-expand-valid-only-p t)
  (defvar helm-ff-sort-expansions-p t)
  (defun helm-ff-try-expand-fname (candidate)
    (let ((dirparts (split-string candidate "/"))
          valid-dir
          fnames)
      (catch 'break
        (while dirparts
          (if (file-directory-p (concat valid-dir (car dirparts) "/"))
              (setq valid-dir (concat valid-dir (pop dirparts) "/"))
            (throw 'break t))))
      (setq fnames (cons candidate (helm-ff-try-expand-fname-1 valid-dir dirparts)))
      (if helm-ff-sort-expansions-p
          (sort fnames
                (lambda (f1 f2) (or (file-directory-p f1)
                                    (not (file-directory-p f2)))))
        fnames)))

  (defun helm-ff-try-expand-fname-1 (parent children)
    (if children
        (if (equal children '(""))
            (and (file-directory-p parent) `(,(concat parent "/")))
          (when (file-directory-p parent)
            (apply 'nconc
                   (mapcar
                    (lambda (f)
                      (or (helm-ff-try-expand-fname-1 f (cdr children))
                          (unless helm-ff-expand-valid-only-p
                            (and (file-directory-p f)
                                 `(,(concat f "/" (mapconcat 'identity
                                                             (cdr children)
                                                             "/")))))))
                    (directory-files parent t (concat "^"
                                                      (regexp-quote
                                                       (car children))))))))
      `(,(concat parent (and (file-directory-p parent) "/")))))

  (defun qjp-helm-ff-try-expand-fname (orig-func &rest args)
    (let* ((candidate (car args))
           (collection (helm-ff-try-expand-fname candidate)))
      (if (and (> (length collection) 1)
               (not (file-exists-p candidate)))
          (with-helm-alive-p
            (when (helm-file-completion-source-p)
              (helm-exit-and-execute-action
               (lambda (_)
                 (helm-find-files-1
                  (helm-comp-read "Expand Path to: " collection))))))
        (apply orig-func args))))

  (advice-add 'helm-ff-kill-or-find-buffer-fname :around #'qjp-helm-ff-try-expand-fname))

(setq quick-launch-dir "~\\AppData\\Roaming\\Microsoft\\Internet Explorer\\Quick Launch")
(defun helm-quick-launch ()
  (interactive)
  (let* ((source (helm-build-sync-source "test"
                   :candidates (cddr (directory-files quick-launch-dir nil ".lnk"))))
         (link (helm :sources source
                     :buffer "*helm sync source*"))
         (full-path (expand-file-name (file-name-as-directory quick-launch-dir))))
    (ruin/async-shell-command-no-output (concat full-path link))))

(provide 'ruin-helm)
