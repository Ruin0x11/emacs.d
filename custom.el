(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((projectile-project-run-cmd . "OpenNefia")
     (projectile-project-compilation-cmd . "OpenNefia")
     (projectile-project-compilation-cmd . "wxLua src/main.lua")
     (projectile-project-compilation-cmd . "elona-next")
     (projectile-project-run-cmd . "elona-next")
     (projectile-project-compilation-cmd "elona-next")
     (projectile-project-run-cmd "elona-next")
     (elona-next-always-send-to-repl . t)
     (eval let
           ((cmd
             (if
                 (eq system-type
                     (quote windows-nt))
                 "cd z:/build/elona-next/src & taskkill /IM love.exe /f & C:\\Users\\kuzuki\\build\\megasource\\build\\love\\Debug\\love.exe --console ." "elona-next")))
           (setq projectile-project-compilation-cmd cmd)
           (setq projectile-project-run-cmd cmd))))))
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
