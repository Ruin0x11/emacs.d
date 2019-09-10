;; ruin-tex.el --- settings for TeX/LaTeX
(setq latex-run-command "pdflatex")

(evil-leader/set-key-for-mode 'latex-mode
  "mb" 'tex-latex-block
  "mf" 'tex-without-changing-windows
  "mc" 'tex-compile
  "me" 'tex-close-latex-block)

(provide 'ruin-tex)
