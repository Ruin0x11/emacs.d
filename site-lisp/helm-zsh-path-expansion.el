;;; -*- lexical-binding: t; -*-

;; see http://cute-jumper.github.io/emacs/2015/11/17/let-helm-support-zshlike-path-expansion

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