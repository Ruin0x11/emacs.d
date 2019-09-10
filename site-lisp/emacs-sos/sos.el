;;; sos.el --- StackOverflow Search

;; Copyright (C) 2014 Rudolf Olah <omouse@gmail.com>

;; Author: Rudolf Olah
;; URL: https://github.com/omouse/emacs-sos
;; Version: 0.1
;; Created: 2014-02-15
;; By: Rudolf Olah
;; keywords: tools, search, questions
;; Package-Requires: ((org "7"))

;; Emacs-SOS is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; Emacs-SOS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs-SOS. If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'cl)
(require 'json)
(require 'url)
(require 'url-http)
(require 'tabulated-list)
(require 'sos-google)

(defvar sos-api-key "6ocCcuEcIyvBvLn44Gh)CQ(("
  "API key for StackExchange.")

(defvar sos-results 'nil
  "Search query results for StackOverflow questions.")

(defvar sos-question-list-window 'nil
  "Current window displaying StackOverflow questions." )
(defvar sos-answer-view-window 'nil
  "Current window displaying StackOverflow answers.")

(defvar sos-question-list-buffer 'nil
  "Current buffer displaying StackOverflow questions.")

(defvar sos-buffer-list 'nil)

(defvar sos-get-answers 'nil
  "If non-nil retrieve and SO's answers to SO's questions when building the search result buffer.
This will slow down the process.")

(defvar sos-answer-code-block-regexp
  "^#\\+BEGIN_CODE\n\\(\\(?:.\\|\n\\)+?\\)\n#\\+END_CODE"
  "Regexp for matching SOS Answer code blocks.")

(defvar sos-google-question-id-regexp
  "stackoverflow.com/questions/\\([0-9]+?\\)/"
  "Regexp for getting question id from a StackOverflow URL.")

(defcustom sos-build-results-function 'sos-build-results-google
  "Function used to get StackOverflow results. Use either
   `sos-build-results-api' or `sos-build-results-google'."
  :type 'symbol
  :group 'sos)

;; www.stackoverflow.com/questions/12345/asdf
;; stackoverflow.com/questions/\\([0-9]+?\\)/


(defun sos-decode-html-entities (str)
  "Decodes HTML entities in a string."
  (let ((result str))
    (loop for entity in '(("&quot;" . "\"")
                          ("&apos;" . "'")
                          ("&#39;" . "'")
                          ("&hellip" . "...")
                          ("&amp;" . "&")
                          ("&gt;" . ">")
                          ("&lt;" . "<")
                          ("&#194;" . "Â")
                          ("&#178;" . "²"))
          do (setq result (replace-regexp-in-string (regexp-quote (car entity)) (cdr entity) result nil 'literal))
          )
    result))

(defun sos-uncompress-callback (&optional status)
  "Callback for url-retrieve that decompresses gzipped content in
the HTTP response. Code taken from
http://stackoverflow.com/a/4124056/9903

Modified for use with url-retrieve-synchronously by making the
`status' argument optional.

Returns the buffer of the uncompressed gzipped content."
  (let ((filename (make-temp-file "download" nil ".gz"))
        (coding-system-for-read  'binary)
        (coding-system-for-write 'binary))
    (search-forward "\n\n") ; Skip response headers.
    (write-region (point) (point-max) filename)
    (with-auto-compression-mode
      (find-file filename))
    (current-buffer)))

(defun sos-get-response-body (buffer)
  "Extract HTTP response body from HTTP response, parse it as JSON, and return the JSON object. `buffer' may be a buffer or the name of an existing buffer.

Modified based on fogbugz-mode, renamed from
`fogbugz-get-response-body':
https://github.com/omouse/fogbugz-mode"
  (set-buffer buffer)
  (switch-to-buffer buffer)
  (let* ((uncompressed-buffer (sos-uncompress-callback))
         (json-response (json-read)))
    (kill-buffer uncompressed-buffer)
    json-response))

(defun sos-insert-search-result (item)
  "Inserts the contents of StackOverflow JSON object, `item',
into the current buffer."
  (let ((id (cdr (assoc 'question_id item))))
    (insert (format "* %s: %s [[http://stackoverflow.com/q/%d][link]]\n"
		    (upcase (subseq (cdr (assoc 'item_type item)) 0 1))
		    (cdr (assoc 'title item))
		    (cdr (assoc 'question_id item)))
	    ":PROPERTIES:\n"
	    ":ID: " (int-to-string id) "\n"
	    ":SO_TAGS: "
	    (reduce
	     (lambda (x y) (format "%s %s" x y))
	     (cdr (assoc 'tags item))) "\n"
	     ":END:\n"
	     (cdr (assoc 'excerpt item))
	     "\n\n** (Read more)\n"
	     (cdr (assoc 'body item))
	     (if (not sos-get-answers) ""
	       (sos-get-answers id))
	     "\n")))

(defun sos-build-results-google (query site)
  "Get tabulated list entries using Google.

   Probably gives the most relevant results. Similar to how2."
  (let* (
         (google-query (concat "site:stackoverflow.com " query " " site))
         (google-results (sos-google--search google-query))
         (count (length google-results))
         entries
         list-results '()
         (index 0))

    (setq tabulated-list-format
          (vector
           '("title" 60 t)
           '("url" 20 t)
           ;; '("score" 10 t)
           ))

    (dotimes (i count)
      (let* ((result (elt google-results i))
             (result-name (plist-get result :title))
             (result-url (plist-get result :url))
             (result-score 0)
             (result-question-id
              (cond ((string-match sos-google-question-id-regexp result-url) (match-string 1 result-url))
                    (t nil))))

        (when result-question-id
          (push (list index (vector

                         result-name
                         result-question-id

                         ))
                entries)
          (setq index (+ 1 index))
          (setq list-results (cons (acons 'question_id result-question-id '()) list-results)))
        )
      )
    (setq sos-results (nreverse list-results))
    (setq tabulated-list-entries (nreverse entries))
    ))

(defun sos-build-results-api (query tag)
  "Get tabulated list entries using the StackExchange API.

   May give less relevant results than using Google."
  (let* ((api-url (concat "http://api.stackexchange.com/2.2/search/excerpts"
                          "?order=desc"
                          "&sort=votes"
                          "&tagged=" tag
                          "&q=" (url-hexify-string query)
                          "&site=stackoverflow"
                          "&key=" sos-api-key))
         (response-buffer (url-retrieve-synchronously api-url))
         (json-response (sos-get-response-body response-buffer))
         (results (cdr (assoc 'items json-response)))
         (count (length results))
         (quota (cdr (assoc 'quota_remaining json-response)))
         entries)

    (setq sos-results results)

    (setq tabulated-list-format
          (vector
           '("title" 80 t)
           '("score" 13 t)
           ;; '("modules" 30 t)
           ;; '("name" 1 t)
           ))

    (setq tabulated-list-sort-key nil)

    (dotimes (i count)
      (let* ((result (elt results i))
             (result-name (sos-decode-html-entities (cdr (assoc 'title result))))
             (result-score (int-to-string (cdr (assoc 'score result)))))

        (push (list i (vector

                       result-name
                       result-score

                       ))
              entries)))
    (message "Quota remaining: %s" (int-to-string quota))
    (setq tabulated-list-entries (nreverse entries))))

(defun sos-build-results (query site)
  "Retrieves a list of StackOverflow results by calling
   `sos-build-results-function'."
  (funcall sos-build-results-function query site))

;;;###autoload
(defun sos (query)
  "Searches StackOverflow for the given `query'. Displays excerpts from the search results.

API Reference: http://api.stackexchange.com/docs/excerpt-search"
  (interactive "sSearch StackOverflow: ")

  ;; set up the buffer
  (if (window-live-p sos-question-list-window)
      (progn
        (select-window sos-question-list-window)
        (kill-buffer sos-question-list-buffer)
        (switch-to-buffer (concat "*sos - " query "*")))
    (switch-to-buffer (concat "*sos - " query "*")))

  (setq sos-question-list-window (selected-window))
  (setq sos-question-list-buffer (current-buffer))

  ;;TODO: bad
  (setq sos-buffer-list (cl-adjoin sos-question-list-buffer sos-buffer-list))

  (sos-mode)
  (erase-buffer)
  (visual-line-mode t)

  (sos-build-results query "")
  (tabulated-list-init-header)
  (tabulated-list-print)
  (goto-char (point-min)))

(defun sos-download-answers (id)
  "Download answers for question ID."
  (let* ((api-url (concat "http://api.stackexchange.com/2.2/"
                          "questions/"
                          (if (stringp id) id
                            (int-to-string id))
                          "/answers"
                          "?order=desc"
                          "&sort=votes"
                          "&filter=withbody"
                          "&site=stackoverflow"
                          "&key=" sos-api-key))
         (response-buffer (url-retrieve-synchronously api-url))
         (json-response (save-window-excursion
                          (switch-to-buffer response-buffer)
                          (goto-char (point-min))
                          (sos-get-response-body response-buffer)))
         (answer-list (cdr (assoc 'items json-response))))
    answer-list))

(defun sos-get-answers (id)
  "Download and format answers for SO's question defined by ID."
  (let* (answer-list (sos-download-answers id))
    (n-answers (length answer-list))
    (i 0)
    (sos-string ""))
  (while (< i n-answers)
    (let* ((answer (elt answer-list i))
           (accepted? (not (eq json-false (cdr (assoc 'is_accepted answer)))))
           (author (cdr (assoc 'owner answer)))
           (author-name (cdr (assoc 'display_name author))))
      (setq sos-string
            (concat sos-string
                    (concat
                     (propertize (concat "Answer " (int-to-string (1+ i)) " by " author-name (if accepted? " (Accepted)" "")
                                         " Score: " (int-to-string (cdr (assoc 'score answer)))
                                         "\n")
                                 'face 'underline 'sos-answer-section t)
                     (cdr (assoc 'body answer))
                     "\n")

                      )
              i (1+ i))))
    sos-string)

(defun sos-current-result ()
  "Returns the current hightlighted StackOverflow question in the tabulated list."
  (elt sos-results (tabulated-list-get-id)))

(defun sos-current-id ()
  (cdr (assoc 'question_id (sos-current-result))))

;;;###autoload
(defun sos-answer ()
  "Get answers for SO question ID as defined in property block of the current question."
  (interactive)
  (let ((id (sos-current-id)))
    (if (not id)
        (message "Cannot see question ID at point.")
      (if (not (window-live-p sos-answer-view-window))
          (setq sos-answer-view-window
                (cond ((with-demoted-errors "Unable to split window: %S"
                         (split-window-vertically 10)))
                      (t ;; no splitting; just use the currently selected one
                       (selected-window)))))
      (select-window sos-answer-view-window)
      (switch-to-buffer "*sos answer*")

      ;;TODO: bad
      (setq sos-buffer-list (cl-adjoin (current-buffer) sos-buffer-list))

      (sos-answer-mode)

      ;; an html2text bug removes "pre" since "p" is a substring
      (make-local-variable 'visual-wrap-col)
      (setq html2text-remove-tag-list (remove "p" html2text-remove-tag-list))
      (setq html2text-remove-tag-list (remove "img" html2text-remove-tag-list))
      (add-to-list 'html2text-remove-tag-list2 "p")

      (add-to-list 'html2text-format-tag-list '("code" . sos-answer-html2text-code) )
      (add-to-list 'html2text-format-tag-list '("pre" . sos-answer-html2text-pre) )

      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert (sos-get-answers id))
        (html2text)
        (goto-char (point-min))

        (while (null (eobp))
        ;; Don't fill pre blocks.
        (unless (sx-question-mode--dont-fill-here)
          (let ((beg (point)))
            (skip-chars-forward "\r\n[:blank:]")
            (forward-paragraph)
            (let ((end (point-marker)))
              (set-marker-insertion-type end t)
              ;; Turn markdown linebreaks into their final form
              (sx-question-mode--process-line-breaks beg end)
              ;; Compactify links by paragraph, so we don't linkify
              ;; inside code-blocks. This will still linkify inside
              ;; code tags, unfortunately.
              (sx-question-mode--process-links beg end)
              ;; Filling is done after all of the above, since those
              ;; steps change the length of text.
              (fill-region beg end)
              (goto-char end)))))
        ))))

(defun sos-questions-move (lines)
  "Move point LINES lines forward (if LINES is positive) or
backward (if LINES is negative). If this succeeds, return the new
docid. Otherwise, return nil."
  (unless (eq major-mode 'sos-mode)
    (error "Must be in SOS mode."))
  (let ((succeeded (zerop (forward-line lines)))
        (current-id (sos-current-id))
        ;; move point, even if this function is called when this window is not
        ;; visible
        (when current-id
          ;; update all windows showing the headers buffer
          (walk-windows
           (lambda (win)
             (when (eq (window-buffer win) sos-question-list-buffer)
               (set-window-point win (point))))
           nil t)
          ;;(set-window-point (get-buffer-window mu4e~headers-buffer t) (point))
          ;; attempt to highlight the new line, display the message
          ;; (mu4e~headers-highlight docid)
          ;; update message view if it was already showing
          (when (and t (window-live-p sos-question-list-window))
            (sos-answer id))
          docid))))

(defun sos-hide-buffer (buffer)
  (let ((window (get-buffer-window buffer)))
    (when window
      (cond ((eq window sos-question-list-window)
             (setq sos-question-list-window nil))
            ((eq window sos-answer-view-window)
             (if (window-live-p sos-question-list-window)
                 (select-window sos-question-list-window))
             (setq sos-answer-view-window nil)))
      (if (> (count-windows) 1)
          (delete-window window)
        (switch-to-buffer (other-buffer)))))
  (when (buffer-live-p buffer) (bury-buffer buffer)))

(defun sos-suspend ()
  (interactive)
  (mapc 'sos-hide-buffer sos-buffer-list))

(defun sos-quit-buffer ()
  (interactive)
  (unless (eq major-mode 'sos-mode)
    (error "Must be in sos-mode"))
  (when (window-live-p sos-answer-view-window)
    (delete-window sos-answer-view-window))
  (kill-buffer))

(defvar sos-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'sos-answer)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map "q" 'sos-quit-buffer)
    map)
  "Keymap used for sos-mode commands.")

(define-derived-mode sos-mode tabulated-list-mode "SOS")

(defface sos-answer-keywords-face
  '((((class color) (background light)) (:foreground "purple"))
    (((class color) (background dark)) :foreground "salmon"))
  "SOS answer mode face used to highlight keywords."
  :group 'sos-answer)

(defface sos-answer-code-block-face
  '((((class color) (background light)) (:foreground "purple"))
    (((class color) (background dark)) (:foreground "light gray" :background "#3E3D31")))
  "SOS answer mode face used to highlight code blocks."
  :group 'sos-answer)

(defun sos-answer-html2text-code (p1 p2 p3 p4)
  (if (not (get-text-property p3 'sos-code-block))
      (put-text-property p2 p3 'face 'sos-answer-keywords-face))
  (html2text-delete-tags p1 p2 p3 p4)
  )

(defun replace-region (from to string)
  "Replace the region specified by FROM and TO to STRING."
  (goto-char from)
  (insert string)
  (delete-char (- to from)))

(defun sos-answer-html2text-pre (p1 p2 p3 p4)
  (put-text-property p2 p3 'face 'sos-answer-code-block-face)
  (put-text-property p2 p3 'sos-code-block t)
  ;; (html2text-delete-tags p1 p2 p3 p4)
  (replace-region p3 p4 (propertize "#+END_CODE" 'face font-lock-comment-face))
  (replace-region p1 p2 (propertize "#+BEGIN_CODE\n" 'face font-lock-comment-face))
  )

(defun sos-goto-property-change (prop &optional direction)
  "Move forward to the next change of text-property PROP.
Return the new value of PROP at point.
If DIRECTION is negative, move backwards instead."
  (let ((func (if (and (numberp direction)
                       (< direction 0))
                  #'previous-single-property-change
                #'next-single-property-change))
        (limit (if (and (numberp direction)
                        (< direction 0))
                   (point-min) (point-max))))
    (goto-char (funcall func (point) prop nil limit))
    (get-text-property (point) prop)))


(defun sos-answer-pos-in-code-block? (pos)
  (get-text-property pos 'sos-code-block))

(defun sos-answer-point-in-code-block? ()
  (sos-answer-pos-in-code-block? (point)))

(defun sos-answer-code-at-point-p ()
  "Match an SOS Answer code block at point.

   Group 1 will contain the matched code block."
  (interactive)
  ;; (unless (sos-answer-point-in-code-block?)
  ;;   (error "Not in SOS code block."))
  (thing-at-point-looking-at sos-answer-code-block-regexp))

(defun sos-answer-yank-code-block ()
  "Adds the Sos Answer code block at point to the kill ring."
  (interactive)
  (if (sos-answer-code-at-point-p)
      (progn
        (copy-region-as-kill (match-beginning 1) (match-end 1))
        (message "Copied code block.")
        )))

(defun sos-answer-next-code-block ()
  (interactive)
  (if (re-search-forward sos-answer-code-block-regexp)
      (goto-char (match-beginning 1)))
  )

(defun sos-answer-previous-code-block ()
  (interactive)
  (if (re-search-backward sos-answer-code-block-regexp)
      (goto-char (match-beginning 1)))
  )

(defun sos-answer-next-section (prop &optional n)
  "Move down to next section (question or answer) of this buffer.
Prefix argument N moves N sections down or up."
  (interactive "p")
  (let ((count (if n (abs n) 1)))
    (while (> count 0)
      ;; This will either move us to the next section, or move out of
      ;; the current one.
      (unless (sos-goto-property-change prop n)
        ;; If all we did was move out the current one, then move again
        ;; and we're guaranteed to reach the next section.
        (sos-goto-property-change prop n))
      (unless (get-char-property (point) 'invisible)
        (cl-decf count))))
  (when (equal (selected-window) (get-buffer-window))
    (recenter 0))
  )

(defun sos-answer-previous-section (prop &optional n)
  "Move down to previous section (question or answer) of this buffer.
Prefix argument moves N sections up or down."
  (interactive "p")
  (sos-answer-next-section prop (- (or n 1))))

(defun sos-answer-next-answer (&optional n)
  (interactive "p")
  (sos-answer-next-section 'sos-answer-section (or n 1)))

(defun sos-answer-previous-answer (&optional n)
  (interactive "p")
  (sos-answer-previous-section 'sos-answer-section (or n 1)))

(defun sos-answer-next-code-block (&optional n)
  (interactive "p")
  (sos-answer-next-section 'sos-code-block (or n 1)))

(defun sos-answer-previous-code-block (&optional n)
  (interactive "p")
  (sos-answer-previous-section 'sos-code-block (or n 1)))

(defun sos-answer-quit-buffer ()
  (interactive)
  (unless (eq major-mode 'sos-answer-mode)
    (error "Must be in sos-answer-mode"))
  (let ((curbuf (current-buffer)) (curwin (selected-window))
        (question-win))
    (walk-windows
     (lambda (win)
       ;; check whether the headers buffer window is visible
       (when (eq sos-question-list-buffer (window-buffer win))
         (setq question-win win))
       ;; and kill any _other_ (non-selected) window that shows the current
       ;; buffer
       (when
           (and
            (eq curbuf (window-buffer win)) ;; does win show curbuf?
            (not (eq curwin win))	    ;; but it's not the curwin?
            (not (one-window-p))) ;; and not the last one on the frame?
         (delete-window win))))  ;; delete it!
    ;; now, all *other* windows should be gone.
    ;; if the headers view is also visible, kill ourselves + window; otherwise
    ;; switch to the headers view
    (if (window-live-p question-win)
        ;; headers are visible
        (progn
          (kill-buffer-and-window) ;; kill the view win
          (setq sos-question-list-window nil)
          (select-window question-win)) ;; and switch to the headers win...
      ;; headers are not visible...
      (progn
        (kill-buffer)
        (setq sos-question-list-window nil)
        (when (buffer-live-p sos-question-list-buffer)
          (switch-to-buffer sos-question-list-buffer))))))

(defvar sos-answer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "q" 'sos-answer-quit-buffer)
    (define-key map "y" 'sos-answer-yank-code-block)
    (define-key map "n" 'sos-answer-next-answer)
    (define-key map "p" 'sos-answer-previous-answer)
    map)
  "Keymap used for sos-mode commands.")

(define-derived-mode sos-answer-mode special-mode "SOS Answer")

;; (add-to-list 'evil-emacs-state-modes 'sos-answer-mode)
(evil-make-overriding-map sos-mode-map 'normal t)
(evil-make-overriding-map sos-answer-mode-map 'normal t)
(ruin/window-movement-for-map sos-answer-mode-map)
(ruin/window-movement-for-map sos-mode-map)

(provide 'sos)

;;; sos.el ends here
