(makunbound 'evil-leader-mode)
(define-minor-mode evil-leader-mode
  "Minor mode to enable <leader> support."
  :init-value nil
  :keymap nil
  (let* ((prefixed (read-kbd-macro (concat evil-leader/non-normal-prefix evil-leader/leader)))
         (no-prefix (read-kbd-macro evil-leader/leader))
	 (minor-modes (mapcar 'car minor-mode-alist))
	 (active-modes (cons major-mode minor-modes))
	 (get-keymap-for-mode (lambda (mode) (cdr (assoc mode evil-leader--mode-maps))))
	 (mode-map (apply 'append (mapcar get-keymap-for-mode active-modes)))
         (map (or mode-map evil-leader--default-map))
         (no-prefix-rx (if evil-leader/no-prefix-mode-rx
                           (mapconcat #'identity evil-leader/no-prefix-mode-rx "\\|")
                         nil)))
    (if evil-leader-mode
        (progn
          (evil-normalize-keymaps)
          (define-key evil-motion-state-local-map no-prefix map)
          (define-key evil-normal-state-local-map no-prefix map)
          (when evil-leader/in-all-states
            (define-key evil-emacs-state-local-map prefixed map)
            (define-key evil-insert-state-local-map prefixed map))
          (when (and no-prefix-rx (string-match-p no-prefix-rx (symbol-name major-mode)))
            (define-key evil-emacs-state-local-map no-prefix map)
            (define-key evil-insert-state-local-map no-prefix map)))
      (define-key evil-motion-state-local-map no-prefix nil)
      (define-key evil-normal-state-local-map no-prefix nil)
      (when evil-leader/in-all-states
        (define-key evil-emacs-state-local-map prefixed nil)
        (define-key evil-insert-state-local-map prefixed nil)
        (when (and no-prefix-rx (string-match-p no-prefix-rx (symbol-name major-mode)))
          (define-key evil-emacs-state-local-map no-prefix nil)
          (define-key evil-insert-state-local-map no-prefix nil))))))
