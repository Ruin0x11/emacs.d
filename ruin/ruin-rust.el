(package-require 'rust-mode)
(package-require 'toml-mode)
(package-require 'flycheck-rust)
(package-require 'cargo)
(package-require 'racer)

(setq
 racer-cmd "~/.cargo/bin/racer"
 racer-rust-src-path "/home/nuko/build/rust/src")

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)

(add-hook 'cargo-process-mode-hook #'turn-on-visual-line-mode)

(evil-leader/set-key-for-mode 'rust-mode
  "dd" 'racer-describe
  "df" 'racer-find-definition

  "fs" 'racer-find-definition

  "kr" 'cargo-process-run
  "kc" 'cargo-process-build
  "kk" 'cargo-process-clean
  "ku" 'cargo-process-update
  "kg" 'ruin/rust-gdb

  "mu" 'ruin/use-missing-imports

  "ta" 'cargo-process-test
  "tt" 'cargo-process-current-test
  "tf" 'ruin/cargo-test-current-mod-or-file
  )

(evil-leader/set-key-for-mode 'toml-mode
  "kr" 'cargo-process-run
  "kc" 'cargo-process-build
  "kk" 'cargo-process-clean
  "ku" 'cargo-process-update
  "kg" 'ruin/rust-gdb

  "ta" 'cargo-process-test
  )

(eval-after-load "rust-mode"
  '(define-key rust-mode-map [f9] 'cargo-process-run))

(sp-local-pair 'rust-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun ruin/rust-scratch-buffer ()
  (interactive)
  (let ((filename (make-temp-file "scratch.rs")))
    (find-file filename)
    (rust-mode)
    (insert-file-contents "~/.emacs.d/misc/scrach-template.rs")
    (setq-local compile-command (concat "rustc " filename " -o /tmp/a.out && /tmp/a.out"))
    (message "Compile and run with M-x 'compile'")))

(defun ruin/rust-gdb ()
  (interactive)
  (let* ((project-root (projectile-project-root))
         (program-name (projectile-default-project-name project-root))
         (target-dir (concat project-root "target/debug"))
         (targets (directory-files target-dir nil (concat program-name "-*")))
         (selected (completing-read "Debug target: " targets nil t))
         (gdb-args (concat "rust-gdb -i=mi " target-dir "/" selected)))
    (gdb gdb-args)))

(defun ruin/cargo-test-current-mod-or-file ()
  "If the file is named mod.rs, run cargo test using parent directory as name. Otherwise use file name."
  (interactive)
  (let* ((dirname (file-name-nondirectory (directory-file-name default-directory)))
        (arg (if (string= (file-name-base) "mod")
                 dirname
               (format "%s::%s" dirname (file-name-base)))))
    (cargo-process--start "Test" (format "cargo test %s"
                                         arg))))

(defun ruin/use-missing-imports ()
  (interactive)
  "Automatically imports all missing dependencies."
  (let* ((errs (ruin/filter-missing flycheck-current-errors))
         (imports (delq nil (delete-dups
                             (cl-remove nil (mapcar 'ruin/use-single errs))))))
    (save-excursion
      (dolist (import imports)
        (goto-char (point-min))
        (insert import "\n")))))

(defun ruin/filter-missing (errs)
  (cl-remove-if-not '(lambda (err) (and (string= (flycheck-error-id err) "E0412")
                                        (string-match-p (regexp-quote "possible candidate")
                                                        (flycheck-error-format err))))
                    errs))

(defun ruin/use-single (err)
  (interactive)
  (-if-let* ((err-id (flycheck-error-id err))
             (err-string (flycheck-error-format err))
             (use-rx (rx "\`" (group (0+ (or (1+ (not (any "\`" "\\"))) (seq "\\" anything)))) "\`"))
             (import (when (string-match use-rx err-string) (match-string 1 err-string))))
      import
    nil))

(provide 'ruin-rust)
