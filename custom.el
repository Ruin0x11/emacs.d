(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clojure-comment-regexp 'clojure--reader-and-comment-regexp)
 '(package-selected-packages
   '(ivy-hydra dmoccur moccur smex counsel-mode counsel doxymacs yari yaml-mode which-key web-mode toml-mode tiny string-inflection ssass-mode srefactor spaceline solarized-theme smeargle sly-quicklisp slim-mode scss-mode rspec-mode robe rmsbolt rg restclient request realgud-node-inspect rainbow-mode racer quickrun py-autopep8 projectile-rails processing-mode prettier-js powershell popwin pandoc-mode pandoc outshine org-bullets org nord-theme multi-term monokai-theme moe-theme mmm-mode minitest magithub magit-popup lua-mode lsp-ui lsp-rust lispyville less kotlin-mode keyfreq kaomoji json-mode js-doc impatient-mode howdoi highlight-symbol highlight-numbers highlight helm-swoop helm-rg helm-projectile helm-org-rifle helm-flycheck helm-flx helm-ag hcl-mode haxe-mode haml-mode gxref gruvbox-theme groovy-mode graphql-mode gradle-mode google-translate google-this go-mode gnuplot glsl-mode gitignore-mode git-timemachine ggtags general function-args format-all forge flymake-less flycheck-rust flycheck-package flycheck-elixir flycheck-color-mode-line flycheck-clang-tidy firestarter fill-column-indicator expand-region exec-path-from-shell evil-surround evil-smartparens evil-org evil-numbers evil-matchit evil-magit evil-leader evil-commentary evil-anzu eval-in-repl enh-ruby-mode elpy elisp-refs dumb-jump dockerfile-mode diminish csv-mode crux company-lsp company-jedi company-c-headers color-theme-sanityinc-tomorrow coffee-mode cmake-mode cmake-ide clang-format cider-eval-sexp-fu cider chruby ccls cargo bundler buffer-move base16-theme auto-yasnippet arduino-mode alchemist actionscript-mode))
 '(safe-local-variable-values
   '((projectile-project-run-cmd . "cd src && love .")
     (projectile-project-compilation-cmd . "cd src && luajit -l boot -e \"require 'test'\"")
     (projectile-project-run-cmd . "cd bin\\Debug && .\\shiolink.exe && E:\\nanika\\ssp.exe")
     (projectile-project-compilation-cmd . "cd bin && cmake .. && cmake --build . && cd Debug && .\\shiolink.exe"))))
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
