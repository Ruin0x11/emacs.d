;;; helm-feature.el --- Helm completion for Cucumber snippets  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Ian Pickering

;; Author: Ian Pickering <ipickering2@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Allows for the insertion of unimplemented Cucumber step definitions.

;;; Code:

(require 'helm)
(require 'ruby-mode)
(require 'feature-mode)

(defun get-feature-snippet-list ()
  (save-some-buffers
   nil (lambda ()
         (string-equal (file-name-extension (buffer-name)) "feature")
         ))
  (let* ((feature-arg feature-default-directory)
         (cuke-opts (list "-d" ))
         (opts-str (mapconcat 'identity cuke-opts " "))
         (cucumber-command (concat (replace-regexp-in-string "{options}" opts-str
                                                             (replace-regexp-in-string "{feature}" feature-arg feature-cucumber-command) t t)))
         (cucumber-output (shell-command-to-string cucumber-command)))
    (with-temp-buffer
      (let (search-result)
        (insert cucumber-output)
        (goto-char (point-min))
        (setq search-result (search-forward-regexp "You can implement step definitions for undefined steps with these snippets:" nil t))
        (if (not search-result)
            '()
          (kill-region (point-min) (point))
          (butlast (cdr (split-string (buffer-string) "\n\n"))))))))

(defun helm-feature-snippet-transformer (candidates _source)
  (cl-loop for i in candidates
           collect (my-fontify-using-faces (my-fontify-ruby i))))

(defun insert-feature-snippet (candidates)
  "Insert feature snippets CANDIDATES at point."
  (let ((keys (if (listp candidates) candidates (list candidates))))
    (insert (s-join "\n\n" candidates))))

(defmacro helm-feature-helmify-action (action name)
  "Wraps the function ACTION in another function named NAME which
passes the candidates marked in helm to ACTION.  Also uses
with-helm-current-buffer such that when ACTION inserts text and
it comes out in the right buffer."
  `(defun ,name (_)
     (let ((keys (helm-marked-candidates :with-wildcard t)))
       (with-helm-current-buffer
         (,action keys)))))

(helm-feature-helmify-action insert-feature-snippet helm-feature-insert-snippet)

(defvar helm-feature-pending-snippets
  `((name . "Cucumber snippets")
    (init . (lambda ()
              (helm-init-candidates-in-buffer
               'local
               (get-feature-snippet-list))))
    (candidates-in-buffer)
    (candidate-number-limit . 50)
    (multiline . t)
    (action . message))
  "Source for completing unimplemented Cucumber snippets.")

(defun my-fontify-ruby (text)
  (with-temp-buffer
    (erase-buffer)
    (insert text)
    (delay-mode-hooks (ruby-mode))
    (font-lock-default-function 'ruby-mode)
    (font-lock-default-fontify-region (point-min)
                                      (point-max)
                                      nil)
    (buffer-string)))

(defun my-fontify-using-faces (text)
  (let ((pos 0))
    (while (setq next (next-single-property-change pos 'face text))
      (put-text-property pos next 'font-lock-face (get-text-property pos 'face text) text)
      (setq pos next))
    (add-text-properties 0  (length text) '(fontified t) text)
    text))


;;;###autoload
(defun helm-feature-snippets ()
  (interactive)
  (helm :sources (helm-build-async-source "Cucumber snippets"
                   :action '(("Insert snippet at point" . helm-feature-insert-snippet))
                   :candidates-process (get-feature-snippet-list)
                   :filtered-candidate-transformer #'helm-feature-snippet-transformer
                   :multiline t)
        :buffer "*helm-feature-snippets*"
        :prompt "snippet: "
        :resume 'noresume))

(provide 'helm-feature)
;;; helm-feature.el ends here
