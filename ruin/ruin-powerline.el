;;; ruin-powerline.el --- modeline/powerline settings

;; (package-require 'powerline)
;; (package-require 'powerline-evil)

;; (require 'powerline)
;; (powerline-evil-center-color-theme)

;; (setq powerline-color1 "#222")      ;; dark grey;
;; (setq powerline-color2 "#444")      ;; slightly lighter grey
;; shape...
;; (setq powerline-arrow-shape 'arrow) ;; mirrored arrows,

(defvar lunaryorn-projectile-mode-line
  '(:propertize
    (:eval (when (ignore-errors (projectile-project-root))
             (concat " " (projectile-project-name))))
    face font-lock-constant-face)
  "Mode line format for Projectile.")
(put 'lunaryorn-projectile-mode-line 'risky-local-variable t)

(setq-default mode-line-format
              '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                ;; Some specific information about the current buffer:
                lunaryorn-projectile-mode-line ; Project information
                (vc-mode vc-mode)
                ;; Misc information, notably battery state and function name
                " "
                mode-line-misc-info
                ;; And the modes, which I don't really care for anyway
                " "
                mode-line-modes
                mode-line-end-spaces))

(provide 'ruin-powerline)
