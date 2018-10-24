;;; sort-regions.el --- sort contiguous regions with an inner regexp  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  Ian Pickering <ipickering2@gmail.com>
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

;;

;;; Code:

(defun sort-regions--next-record ()
  )

(defun sort-regions--end-record ()
  )

(defun sort-regions--start-key ()
  (concat ""))

(defun sort-regions (reverse beg end)
  (interactive "P\nr")
  (let ((regexp "test = \\([0-9]+\\)"))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (sort-subr reverse
                   'sort-regions--next-record
                   'sort-regions--end-record
                   'sort-regions--start-key)
        ))
    ))

(provide 'sort-regions)
;;; sort-regions.el ends here
