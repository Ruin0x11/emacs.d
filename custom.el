(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-interactive-popup-errors nil)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation turn-on-haskell-doc)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(package-selected-packages
   (quote
    (clang-format hcl-mode cmake-mode kotlin-mode lsp-ui lsp-mode evil-numbers tidy skewer-mode org-mind-map rainbow-blocks slim-mode rubocop alchemist elixir-mix elixir-mode ggtags powershell prettier-js js-doc company-jedi jedi py-autopep8 slamhound gnuplot elisp-refs helm-org-rifle eval-in-repl textile-mode lispy lispyville howdoi restclient sed-mode general Omnisharp omnisharp csv-mode yasnippet yari yaml-mode wgrep toml-mode smeargle scss-mode ruby-block rainbow-mode racer processing-mode powerline-evil popwin org-bullets multi-term mmm-mode magit lua-mode lively keyfreq kaomoji jabber highlight-symbol helm-swoop helm-projectile helm-flycheck helm-flx helm-ag haml-mode google-translate google-this go-mode glsl-mode gitignore-mode git-timemachine ghc flycheck-rust flycheck-haskell firestarter fill-column-indicator feature-mode expand-region exec-path-from-shell evil-surround evil-smartparens evil-org evil-multiedit evil-matchit evil-leader evil-jumper evil-commentary enh-ruby-mode emojify dumb-jump diminish dictionary csharp-mode crux company-emoji coffee-mode cider-eval-sexp-fu cider chruby cargo bundler buffer-move arduino-mode anzu ample-theme aggressive-indent)))
 '(safe-local-variable-values
   (quote
    ((flycheck-disabled-checkers quote
                                 (rust rust-cargo))
     (eval progn
           (pp-buffer)
           (indent-buffer))
     (eval progn
           (lispyville-mode 1)
           (smartparens-mode 1))
     (cider-cljs-lein-repl . "(do (user/go) (user/cljs-repl))")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend")
     (flycheck-checker . csharp-hand)
     (default-directory eval
       (projectile-root))
     (flycheck-checker . csharp-unity)
     (flycheck-checker quote csharp-unity)
     (projectile-project-run-cmd lambda nil
                                 (let
                                     ((default-directory "D:\\KISS\\CM3D2")
                                      (display-buffer-alist
                                       (cons
                                        (cons "\\*Async Shell Command\\*.*"
                                              (cons
                                               (function display-buffer-no-window)
                                               nil))
                                        display-buffer-alist)))
                                   (when
                                       (window-valid-p popwin:popup-window)
                                     (popwin:close-popup-window))
                                   (message "Running...")
                                   (async-shell-command "D:\\KISS\\CM3D2\\CM3D2x64.exe")))
     (projectile-project-run-cmd lambda nil)
     (projectile-project-run-cmd lambda nil
                                 (let
                                     ((default-directory "D:\\KISS\\CM3D2")
                                      (display-buffer-alist
                                       (cons
                                        (cons "\\*Async Shell Command\\*.*"
                                              (cons
                                               (function display-buffer-no-window)
                                               nil))
                                        display-buffer-alist)))
                                   (message "Running...")
                                   (async-shell-command "D:\\KISS\\CM3D2\\CM3D2x64.exe")))
     (firestarter . "lua %p"))))
 '(send-mail-function (quote smtpmail-send-it)))
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
