;;; sos-google.el --- Generalized library for scraping Google searches

;; Copyright (C) 2016, Ian Pickering

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Generalized library for scraping Google searches.
;; Shamelessly lifted from helm-google by steckerhalter.

;;; Code:
;; "?q=\\(.+?\\)&"

(defgroup sos-google '()
  "Customization group for `sos-google'."
  :link '(url-link "http://github.com/steckerhalter/sos-google")
  :group 'convenience
  :group 'comm)

(defcustom sos-google-tld "com"
  "The TLD of the google url to be used (com, de, fr, co.uk etc.)."
  :type 'string
  :group 'sos-google)

(defcustom sos-google-use-regexp-parsing nil
  "Force use of regexp html parsing even if libxml is available."
  :type 'boolean
  :group 'sos-google)


(defvar sos-google-input-history nil)
(defvar sos-google-pending-query nil)

(defun sos-google-url ()
  "URL to google searches.
If 'com' TLD is set use 'encrypted' subdomain to avoid country redirects."
  (concat "https://"
          (if (string= "com" sos-google-tld)
              "encrypted"
            "www")
          ".google."
          sos-google-tld
          "/search?ie=UTF-8&oe=UTF-8&q=%s"))

(defun sos-google--process-html (html)
  (replace-regexp-in-string
   "\n" ""
   (with-temp-buffer
     (insert html)
     (html2text)
     (buffer-substring-no-properties (point-min) (point-max)))))

(defmacro sos-google--with-buffer (buf &rest body)
  (declare (doc-string 3) (indent 2))
  `(with-current-buffer ,buf
     (set-buffer-multibyte t)
     (goto-char url-http-end-of-headers)
     (prog1 ,@body
       (kill-buffer ,buf))))

(defun sos-google--parse-w/regexp (buf)
  (sos-google--with-buffer buf
      (let (results result)
        (while (re-search-forward "class=\"r\"><a href=\"/url\\?q=\\(.*?\\)&amp;sa" nil t)
          (setq result (plist-put result :url (match-string-no-properties 1)))
          (re-search-forward "\">\\(.*?\\)</a></h3>" nil t)
          (setq result (plist-put result :title (sos-google--process-html (match-string-no-properties 1))))
          (re-search-forward "class=\"st\">\\([\0-\377[:nonascii:]]*?\\)</span>" nil t)
          (setq result (plist-put result :content (sos-google--process-html (match-string-no-properties 1))))
          (add-to-list 'results result t)
          (setq result nil))
        results)))

(defun sos-google--tree-search (tree)
  (pcase tree
    (`(,x . ,y) (or (and (null y) nil)
                    (and (eql x 'div)
                         (string= (xml-get-attribute tree 'id) "ires")
                         (pcase-let* ((`(_ _ . ,ol) tree)
                                      (`(_ _ . ,items) (car ol)))
                           items))
                    (sos-google--tree-search x)
                    (sos-google--tree-search y)))))

(defun sos-google--parse-w/libxml (buf)
  (let* ((xml (sos-google--with-buffer buf
                  (libxml-parse-html-region
                   (point-min) (point-max))))
         (items (sos-google--tree-search xml))
         (get-string (lambda (element)
                       (mapconcat (lambda (e)
                                    (if (listp e) (car (last e)) e))
                                  element "")))
         (fix-url (lambda (str)
                    (concat "https://www.google." sos-google-tld str)))
         results)
    (dolist (item items results)
      (add-to-list 'results
                   (list :title (funcall get-string (cddr (assoc 'a (assoc 'h3 item))))
                         :cite (funcall get-string (cddr (assoc 'cite (assoc 'div (assoc 'div item)))))
                         :url (funcall fix-url (cdr (assoc 'href (cadr (assoc 'a (assoc 'h3 item))))))
                         :content (sos-google--process-html
                                   (funcall get-string (cddr (assoc 'span (assoc 'div item))))))
                   t))))

(defun sos-google--parse (buf)
  "Extract the search results from BUF."
  (if (or sos-google-use-regexp-parsing
          (not (fboundp 'libxml-parse-html-region)))
      (sos-google--parse-w/regexp buf)
    (sos-google--parse-w/libxml buf)))

(defun sos-google--response-buffer-from-search (text &optional search-url)
  (let ((url-mime-charset-string "utf-8")
        (url (format (or search-url (sos-google-url)) (url-hexify-string text))))
    (url-retrieve-synchronously url t)))

(defun sos-google--search (text)
  (let* ((buf (sos-google--response-buffer-from-search text))
         (results (sos-google--parse buf)))
    results))

(provide 'sos-google)

;;; sos-google.el ends here
