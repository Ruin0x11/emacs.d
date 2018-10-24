;;; ruin-elixir --- elixir support
(package-require 'alchemist)
(package-require 'flycheck-elixir)


(add-hook 'elixir-mode-hook 'flycheck-mode)
(add-hook 'elixir-mode-hook 'alchemist-mode)

(evil-leader/set-key-for-mode 'elixir-mode
  "ta" 'alchemist-mix-test
  "tt" 'alchemist-mix-test-at-point
  "tb" 'alchemist-mix-test-this-buffer
  "tr" 'alchemist-mix-rerun-last-test
  "tf" 'alchemist-project-run-tests-for-current-file
  "tg" 'alchemist-project-toggle-file-and-tests
  "ts" 'alchemist-mix-test-stale
  "fs" 'alchemist-goto-definition-at-point

  "dd" 'alchemist-help-search-at-point

  "mi" 'alchemist-iex-project-run
  "ms" 'alchemist-iex-send-current-line
  "mS" 'alchemist-iex-send-current-line-and-go

  "ee" 'alchemist-eval-current-line
  "eb" 'alchemist-eval-buffer)

;(define-key 'elixir-mode-map (kbd "C-x =") 'alchemist-eval-region)

(provide 'ruin-elixir)
;;; ruin-elixir.el ends here
