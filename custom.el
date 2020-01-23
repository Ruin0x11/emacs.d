(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#BBBBBB"])
 '(compilation-message-face (quote default))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (ascii-art-to-unicode basic-mode lsp-ui yari yaml-mode writeroom-mode which-key web-mode toml-mode tiny ssass-mode srefactor spaceline solarized-theme smex smeargle sly-quicklisp slim-mode scss-mode rspec-mode robe rmsbolt rg restclient request realgud rainbow-mode racer quickrun py-autopep8 projectile-rails processing-mode prettier-js powershell popwin outshine org-bullets nord-theme multi-term monokai-theme moe-theme minitest lua-mode lispyville less kotlin-mode keyfreq kaomoji js-doc ivy-hydra impatient-mode howdoi highlight-symbol highlight-numbers helm-swoop helm-rg helm-projectile helm-org-rifle helm-flycheck helm-flx helm-ag hcl-mode haml-mode gxref groovy-mode graphql-mode google-translate google-this gnuplot glsl-mode gitignore-mode git-timemachine ggtags general function-args format-all flymake-less flycheck-rust flycheck-package flycheck-elixir flycheck-color-mode-line flycheck-clang-tidy firestarter find-file-in-project fill-column-indicator fennel-mode expand-region exec-path-from-shell evil-surround evil-smartparens evil-org evil-numbers evil-matchit evil-magit evil-leader evil-commentary evil-anzu eval-in-repl enh-ruby-mode elpy elisp-refs dumb-jump dockerfile-mode diminish crux company-racer company-jedi company-c-headers color-theme-sanityinc-tomorrow coffee-mode cmake-mode clang-format cider-eval-sexp-fu cider chruby ccls cargo bundler buffer-move base16-theme auto-yasnippet arduino-mode alchemist actionscript-mode)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values
   (quote
    ((eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))
     (eval let
           ((cmd
             (if
                 (eq system-type
                     (quote windows-nt))
                 "cd z:/build/elona-next/src & taskkill /IM love.exe /f & C:\\Users\\kuzuki\\build\\megasource\\build\\love\\Debug\\love.exe --console ." "elona-next")))
           (setq-local projectile-project-compilation-cmd cmd)
           (setq-local projectile-project-run-cmd cmd))
     (eval let
           ((cmd
             (if
                 (eq system-type
                     (quote windows-nt))
                 "cd z:/build/elona-next/src & taskkill /IM love.exe /f & C:\\Users\\kuzuki\\build\\megasource\\build\\love\\Debug\\love.exe --console ." "elona-next")))
           (setq projectile-project-compilation-cmd cmd)
           (setq projectile-project-run-cmd cmd))
     (projectile-project-run-cmd eval
                                 (if
                                     (eq system-type
                                         (quote windows-nt))
                                     "cd z:/build/elona-next/src & taskkill /IM love.exe /f & C:\\Users\\kuzuki\\build\\megasource\\build\\love\\Debug\\love.exe --console ." "elona-next"))
     (projectile-project-compilation-cmd eval
                                         (if
                                             (eq system-type
                                                 (quote windows-nt))
                                             "cd z:/build/elona-next/src & taskkill /IM love.exe /f & C:\\Users\\kuzuki\\build\\megasource\\build\\love\\Debug\\love.exe --console ." "elona-next"))
     (projectile-project-run-cmd if
                                 (eq system-type
                                     (quote windows-nt))
                                 "cd z:/build/elona-next/src & taskkill /IM love.exe /f & C:\\Users\\kuzuki\\build\\megasource\\build\\love\\Debug\\love.exe --console ." "elona-next")
     (projectile-project-compilation-cmd if
                                         (eq system-type
                                             (quote windows-nt))
                                         "cd z:/build/elona-next/src & taskkill /IM love.exe /f & C:\\Users\\kuzuki\\build\\megasource\\build\\love\\Debug\\love.exe --console ." "elona-next")
     (projectile-project-run-cmd . "cd z:/build/elona-next/src & taskkill /IM love.exe /f & C:\\Users\\kuzuki\\build\\megasource\\build\\love\\Debug\\love.exe --console .")
     (projectile-project-compilation-cmd . "cd z:/build/elona-next/src & taskkill /IM love.exe /f & C:\\Users\\kuzuki\\build\\megasource\\build\\love\\Debug\\love.exe --console ."))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#BBBBBB" "#F8F8F0"))))
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
