;;; this-to-that.el --- execute shell command on buffer update  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Ian Pickering

;; Author: Ian Pickering <ianpickering@vpn-165-124-167-3.vpn.northwestern.edu>
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

;; A command to run a shell command on a buffer in one window and see
;; the result in another window in real time.

;; %b - absolute path to buffer

;;; Code:

(require 'format-spec)
(require 'ansi-color)

(defgroup this-to-that nil
  "TODO"
  :group 'convenience
  :prefix "this-to-that-")

(defvar this-to-that--target-mode 'ruby-mode)
(defvar this-to-that--timer nil)
(defvar this-to-that--command nil)
(make-variable-buffer-local 'this-to-that--command)

(defvar this-to-that--target-buffer nil)
(make-variable-buffer-local 'this-to-that--command)

(defcustom this-to-that-buffer-name "*this to that*"
  "Buffer name of the reporting buffer for shell commands."
  :type 'string
  :group 'this-to-that)

(defcustom this-to-that-delay 0.5
  "Delay until this-to-that--command is run.")

(defvar this-to-that--process nil
  "Process associated with current buffer.")
(make-variable-buffer-local 'this-to-that-process)

(defun this-to-that--report (process)
  (let ((output (process-get process 'output))
        end)
    (with-current-buffer (get-buffer-create this-to-that-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;(funcall this-to-that--target-mode)
        (goto-char (point-max))
        (insert
         (ansi-color-apply
          (format-spec "%s"
                       (format-spec-make ?s output)))
         )
        (setq end (point-max))))
    (let ((window (display-buffer this-to-that-buffer-name)))
      (when window
        (set-window-point window end)))))

(defun this-to-that--format (string)
  "Apply format codes on STRING.
Available format codes are:

%b: Buffer name.  Equals the file name for buffers linked with
 files.  Beware that this is merely convention and buffers can be
 renamed to conform to their unique name constraint!

%p: Full path of the file associated with the buffer.  Decomposes
 into a directory and file name part.  If there is no file
 association, the value is an empty string.  As the following
 format codes are directly derived from this value, the same
 caveat applies to them as well.

%d: Directory name of the file associated with the buffer.
 Equals the full path without the file name.

%f: File name of the file associated with the buffer.  Decomposes
 into a file stem and a file extension.

%s: File stem of the file associated with the buffer.  Equals the
 file name without its extension.

%e: File extension of the file associated with the buffer.
 Equals the file name without its stem.  Includes the period if
 an extension is present, otherwise the value is an empty
 string."
  (let* ((buffer (shell-quote-argument (buffer-name)))
         (path (shell-quote-argument (or (buffer-file-name) "")))
         (directory (shell-quote-argument (file-name-directory (or path ""))))
         (file (shell-quote-argument (file-name-nondirectory (or path ""))))
         (stem (shell-quote-argument (file-name-sans-extension file)))
         (extension (shell-quote-argument (file-name-extension file t))))
    (format-spec string (format-spec-make ?b buffer ?p path ?d directory
                                          ?f file ?s stem ?e extension))))


(defun this-to-that--filter (process output)
  "Special process filter.
Appends OUTPUT to the process output property."
  (process-put process 'output (concat (process-get process 'output) output)))

(defun this-to-that--sentinel (process _type)
  "Special process sentinel.
It retrieves the status of PROCESS, then sets up and displays the
reporting buffer."
  (when (memq (process-status process) '(exit signal nil))
    (this-to-that--report process)))

(defun this-to-that--run-command (command &optional type)
  "Execute COMMAND in a shell."
  (if (and this-to-that--process
           (not (memq (process-status this-to-that--process)
                      '(exit signal nil))))
      ;;(error "Process already running")
      (delete-process this-to-that--process)
    (setq this-to-that--process
          (start-process "this-to-that" nil
                         shell-file-name shell-command-switch
                         (this-to-that--format command)))
    (process-put this-to-that--process 'output "")
    (set-process-filter this-to-that--process 'this-to-that--filter)
    (set-process-sentinel this-to-that--process 'this-to-that--sentinel)))

(defun this-to-that--update (&rest args)
  (interactive)
  (when this-to-that--command
    (cond
     ((stringp this-to-that--command)
      (this-to-that--run-command this-to-that--command))
     ((functionp this-to-that--command)
      (call-interactively this-to-that--command))
     ((listp this-to-that--command)
      (eval this-to-that--command))
     (t (error "Invalid value for `this-to-that--command': %s" this-to-that--command)))))

(defun this-to-that--auto-update ()
  (when (equalp this-to-that--target-buffer (current-buffer))
    (save-buffer)
    (this-to-that--update)))

(defun this-to-that--start ()
  (setq this-to-that--command (read-string "Command (%b %p %d %f %s %e): ")
        this-to-that--target-buffer (current-buffer))
  ;(add-hook 'after-change-functions 'this-to-that--auto-update)
  (this-to-that--start-timer)
  (message "This-to-that started"))

(defun this-to-that--stop ()
  ;(remove-hook 'after-change-functions 'this-to-that--auto-update)
  (this-to-that--stop-timer)
  (message "This-to-that stopped"))

(defun this-to-that--start-timer ()
  "start timer."
  (when this-to-that--timer
    (this-to-that--stop-timer))
  (setq this-to-that--timer
        (run-with-idle-timer this-to-that-delay t 'this-to-that--auto-update)))

(defun this-to-that--stop-timer ()
  "Stop timer."
  (when this-to-that--timer
    (cancel-timer this-to-that--timer)
    (setq this-to-that--timer nil)))

;;;###autoload
(define-minor-mode this-to-that-mode
  :lighter " TTT"
  (if (bound-and-true-p this-to-that-mode)
      (this-to-that--start)
    (this-to-that--stop)))


(provide 'this-to-that)
;;; this-to-that.el ends here
