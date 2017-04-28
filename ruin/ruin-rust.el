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

  "kR" 'cargo-process-run
  "kC" 'cargo-process-build
  "kk" 'cargo-process-clean
  "ta" 'cargo-process-test
  "tt" 'cargo-process-current-test
  "tf" 'cargo-process-current-file-tests
  )

(evil-leader/set-key-for-mode 'toml-mode
  "kR" 'cargo-process-run
  "kb" 'cargo-process-build
  "kk" 'cargo-process-clean
  "ta" 'cargo-process-test
  "tf" 'cargo-process-current-file-tests
  )

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

(provide 'ruin-rust)
