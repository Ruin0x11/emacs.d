;; warning: unused import\(s\|\): \(.*\)
;; ^\([0-9]+\) *| use \(.*\);
;;"^\\([0-9]+\\) *|\\( use .*\\);\n +|\\([\s^]*\\)$"

(require 'json)
(require 'projectile)

(defcustom rust-autocorrect-command "cargo check --message-format=json"
  "The default cargo command for \\[rust-autocorrect]."
  :type 'string
  :group 'rust-autocorrect)

(defun rust-autocorrect-process-setup ()
  "Setup compilation variables and buffer for `hlint'."
  (run-hooks 'rust-autocorrect-setup-hook))

(defun rust-autocorrect-finish-hook (buf msg)
  "Function, that is executed at the end of cargo check execution"
  (rust-autocorrect-replace-suggestions))

(define-compilation-mode rust-autocorrect-mode "Rust Autocorrect"
  "Mode for check Haskell source code."
  (set (make-local-variable 'compilation-process-setup-function)
       'rust-autocorrect-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-scroll-output) nil)
  (set (make-local-variable 'compilation-finish-functions)
       (list 'rust-autocorrect-finish-hook)))

(defun rust-autocorrect-replace-suggestions ()
  "Perform actual replacement of suggestions"
  (interactive)
  (goto-char (point-min))
  (let ((suggestions (rust-autocorrect--parse-suggestions))
        ;; If a replacement reduces a line/col count, keep track of them here
        (changed-col-sizes (make-hash-table))
        (changed-line-sizes (make-hash-table)))
    (projectile-save-project-buffers)
    (dolist (sug suggestions)
      (dolist (span (plist-get sug :spans))
        (let* ((filename (plist-get span :filename))
               (text (nth 0 (plist-get span :texts)))
               (mes (format "Remove \"%s\"?" text)))
          (save-excursion
            (when (file-exists-p filename)
              (find-file filename)
              (let ((column (plist-get span :start-column))
                    (line (plist-get span :start-line)))
                (goto-char (point-min))
                (forward-line (- line 1))
                (forward-char (- column 1))
                (re-search-forward (regexp-quote text) (line-end-position))
                (when (match-string 0)
                  (let ((start (match-beginning 0))
                        (end (match-end 0)))
                    (goto-char start)
                    (setf
                     (point) start
                     (mark) end)
                    (when (yes-or-no-p mes)
                      ;; replace match just won't work
                      (kill-region start end)
                      ;; (replace-match "" nil nil)
                      )
                    (deactivate-mark)))
                )
              )
            )
          ))
      )
    ))

(defun rust-autocorrect ()
  "Run cargo check for current project"
  (interactive)
  ;; (save-some-buffers hs-lint-save-files)
  (compilation-start rust-autocorrect-command 'rust-autocorrect-mode))

(require 'cl)

(defun rust-autocorrect--run-command ()
  (shell-command-to-string rust-autocorrect-command))

(defun rust-autocorrect--parse-suggestions ()
  (let* ((output (rust-autocorrect--run-command))
         (plists (rust-autocorrect--parse-cargo-output output)))
    (mapcar 'rust-autocorrect--convert-message plists)))

(defun rust-autocorrect--parse-cargo-output (output)
  (let* ((lines (split-string output "\n" t))
         (json-lines (remove-if-not (lambda (s) (string-prefix-p "{\"" s)) lines)))
    (remove-if-not (lambda (s) (assoc 'message s))
                   (mapcar 'json-read-from-string json-lines))))

                                        ;(rust-autocorrect--convert-message (nth 0 objs))

(defun rust-autocorrect--convert-text (text)
  (let* ((start (- (cdr (assoc 'highlight_start text)) 1))
         (end (- (cdr (assoc 'highlight_end text)) 1))
         (text-str (cdr (assoc 'text text))))
    (substring text-str start end)))

(defun rust-autocorrect--convert-span (span)
  (let* ((filename (cdr (assoc 'file_name span)))
         (column (cdr (assoc 'column_start span)))
         (line (cdr (assoc 'line_start span)))
         (texts (mapcar 'rust-autocorrect--convert-text (cdr (assoc 'text span)))))
    (list :filename filename
          :start-line line
          :start-column column
          :texts texts)))

(defun rust-autocorrect--convert-message (mes)
  (let* ((mes-obj (cdr (assoc 'message mes)))
         (spans (mapcar 'rust-autocorrect--convert-span (cdr (assoc 'spans mes-obj))))
         (mes-str (cdr (assoc 'message mes-obj))))
    (list :message mes-str
          :spans spans)))

(provide 'rust-autocorrect)
