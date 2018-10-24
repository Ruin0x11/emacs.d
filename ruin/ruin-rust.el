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
(add-hook 'rust-mode-hook #'rust-enable-format-on-save)

;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)

(add-hook 'cargo-process-mode-hook #'turn-on-visual-line-mode)

(evil-leader/set-key-for-mode 'rust-mode
  "dd" 'racer-describe
  "df" 'racer-find-definition

  "bi" 'rust-format-buffer

  "fs" 'racer-find-definition

  "kg" 'ruin/rust-gdb

  "mu" 'ruin/use-missing-imports
  "mk" 'cargo-process-clippy

  "ta" 'cargo-process-test
  "tt" 'ruin/my-cargo-process-current-test
  "tf" 'ruin/cargo-test-current-mod-or-file
  "ts" 'ruin/cargo-test-tests
  )

(eval-after-load "rust-mode"
  '(define-key rust-mode-map [f9] 'cargo-process-check))

;; (sp-local-pair 'rust-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

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

(defun ruin/cargo-test-tests ()
  "Test all cargo tests."
  (interactive)
  (cargo-process--start "Test" "cargo test --tests"))

(defun ruin/cargo-test-current-mod-or-file ()
  "If the file is named mod.rs, run cargo test using parent directory as name. Otherwise use file name."
  (interactive)
  (let* ((dirname (file-name-nondirectory (directory-file-name default-directory)))
         (arg (if (string= (file-name-base) "mod")
                  dirname
                (format "%s::%s" dirname (file-name-base)))))
    (cargo-process--start "Test" (format "cargo test %s"
                                         arg))))

(defun ruin/my-cargo-process-current-test ()
  "Run the Cargo test command for the current test.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests."
  (interactive)
  (cargo-process--start "Test" (format "cargo test %s"
                                       (cargo-process--get-current-test))))


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

;; prevent flycheck from blocking cargo subprocesses
(defun kill-flycheck ()
  (when (flycheck-running-p)
    (flycheck-stop)))

(dolist (func '(cargo-process-test
                ruin/cargo-test-current-mod-or-file
                ruin/my-cargo-process-current-test
                cargo-process-run
                cargo-process-build))
  (advice-add func :after #'kill-flycheck))

(provide 'ruin-rust)
