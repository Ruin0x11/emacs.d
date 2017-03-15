(package-require 'rust-mode)
(package-require 'toml-mode)
(package-require 'flycheck-rust)
(package-require 'cargo)
(package-require 'racer)

(setq
 racer-cmd "~/.cargo/bin/racer"
 racer-rust-src-path "/Users/ruin/build/rust/src")

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook 'cargo-minor-mode)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(evil-leader/set-key-for-mode 'rust-mode
  "dd" 'racer-describe
  "df" 'racer-find-definition

  "fs" 'racer-find-definition

  "kr" 'cargo-process-run
  "kc" 'cargo-process-build
  "kk" 'cargo-process-clean
  "tt" 'cargo-process-test)

(evil-leader/set-key-for-mode 'toml-mode
  "kr" 'cargo-process-run
  "kb" 'cargo-process-build
  "kc" 'cargo-process-clean
  "tt" 'cargo-process-test)

;; automatically indent braces
(sp-local-pair 'rust-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(provide 'ruin-rust)
