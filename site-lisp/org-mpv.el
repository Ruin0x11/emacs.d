;;; org-mpv.el - Support for links to things with mpv

(require 'org)

(org-add-link-type "mpv" 'org-mpv-open)

(defcustom org-mpv-command 'mpv
  "The Emacs command to be used to display a mpv page."
  :group 'org-link
  :type '(const mpv))

(defun org-mpv-open (path)
  "Visit the manpage on PATH.
PATH should be a topic that can be thrown at the mpv command."
  (let (file position start end)
    (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
        (error "Error in mpv link"))
    (setq file (match-string 1 path)
          pos (match-string 3 path))

    (if (string-match "\\([^#]+\\)#\\(\\(.+\\)\\)?" pos)
        (setq start (match-string 1 pos)
              end (match-string 2 pos))
      (setq start pos
            end "-1"))
    (org-mpv-follow-link file start end)))

(defun org-mpv-follow-link (file start end)
  (if (null start) (setq start "0"))
  (setq file (concat "\"" file "\""))
  (shell-command (concat "mpv " file " --start=" start " --end=" end " --force-window") nil 0))

(provide 'org-mpv)

;;; org-mpv.el ends here
