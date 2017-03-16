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

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(evil-leader/set-key-for-mode 'rust-mode
  "dd" 'racer-describe
  "df" 'racer-find-definition

  "kR" 'cargo-process-run
  "kb" 'cargo-process-build
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

(provide 'ruin-rust)
