;;; sos-helm.el --- Helm interface for SOS

;; Copyright (C) 2017  Ian Pickering

;; Author: Ian Pickering <ipickering2@gmail.com>
;; Keywords: tools

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

;; Searches for and extracts code from StackOverflow.

;;; Code:

(require 'sos)

(defun sos-helm--extract-result-id (result)
  "Extracts the id from the sos result RESULT."
  (aref (cadr result) 1))

(defun sos-helm--get-answers (query)
  "Downloads the answers from StackOverflow from the search string QUERY."
  (let* ((results (sos-build-results query ""))
         (ids (mapcar 'sos-helm--extract-result-id results))
         (answers (mapconcat 'sos-download-answers ids nil)))
    answers))

(defun sos-helm--get-code-blocks (query)
  "Extracts code blocks from StackOverflow search QUERY."
  (let* ((answers (sos-helm--get-answers query))
         (bodies (mapcar 'sos-helm--extract-answer-body answers))
         (codes (mapcar 'sos-helm--extract-body-code bodies)))
    codes))

(defun sos-helm--extract-answer-body (answer)
  "Extract the body from StackOverflow answer ANSWER."
  (alist-get 'body answer))

(defvar sos-helm--code-tag-regexp
  "<pre><code>\\([^<]*\\)</code></pre>"
  "Regexp for extracting code between <pre><code> tags.")

(defun sos-helm--re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun sos-helm--extract-body-code (body)
  "Extracts code blocks between <code><pre> tags in the HTML string BODY."
  (sos-helm--re-seq sos-helm--code-tag-regexp body))


(provide 'sos-helm)
;;; sos-helm.el ends here
