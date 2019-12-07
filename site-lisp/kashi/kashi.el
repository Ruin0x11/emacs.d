;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'mpc)

(defvar kashi-buffer nil)
(defvar kashi-song-status nil)
(defvar kashi-status-callbacks
  '((state . kashi--status-timers-refresh)
    (song  . kashi--get-lyrics)
    ))

(defun kashi ()
  (interactive)

  (switch-to-buffer-other-window "*kashi*")
  (setq kashi-buffer (current-buffer))

  (kashi--status-refresh)
  )

(defun kashi--get-lyrics ()
  (interactive)
  (message "go")
  (let ((artist (cdr (assq 'Artist kashi-song-status)))
        (title (cdr (assq 'Title kashi-song-status))))
    (kashi--minilyrics artist title)))

(defun kashi--update-buffer (content)
  (with-current-buffer kashi-buffer
    (save-excursion
      (erase-buffer)
      (insert content))))

(defun kashi--status-refresh (&optional callback)
  (interactive)
  (let ((cb callback))
    (mpc-proc-cmd (mpc-proc-cmd-list '("status" "currentsong"))
                  (lambda ()
                    (kashi--mpc-callback))
                  )))

(defun kashi--mpc-callback ()
  (let ((old-status kashi-song-status))
    ;; Update the alist.
    (setq kashi-song-status (mpc-proc-buf-to-alist))
    (cl-assert kashi-song-status)
    (unless (equal old-status kashi-song-status)
      ;; Run the relevant refresher functions.
      (dolist (pair kashi-status-callbacks)
        (when (or (eq t (car pair))
                  (not (equal (cdr (assq (car pair) old-status))
                              (cdr (assq (car pair) kashi-song-status)))))
          (funcall (cdr pair)))))))


(defvar kashi--status-timer nil)
(defun kashi--status-timer-start ()
  (add-hook 'pre-command-hook 'kashi--status-timer-stop)
  (unless kashi--status-timer
    (setq kashi--status-timer (run-with-timer 1 1 'kashi--status-timer-run))))
(defun kashi--status-timer-stop ()
  (when kashi--status-timer
    (cancel-timer kashi--status-timer)
    (setq kashi--status-timer nil)))
(defun kashi--status-timer-run ()
  (with-demoted-errors "kashi: %S"
    (when (process-get (mpc-proc) 'ready)
      (let* ((buf kashi-buffer)
             (win (get-buffer-window buf t)))
        (if (not win)
            (kashi--status-timer-stop)
          (with-local-quit (kashi--status-refresh)))))))


(defvar kashi--status-idle-timer nil)
(defun kashi--status-idle-timer-start ()
  (when kashi--status-idle-timer
    ;; Turn it off even if we'll start it again, in case it changes the delay.
    (cancel-timer kashi--status-idle-timer))
  (setq kashi--status-idle-timer
        (run-with-idle-timer 1 t 'kashi--status-idle-timer-run))
  ;; Typically, the idle timer is started from the kashi--status-callback,
  ;; which is run asynchronously while we're already idle (we typically
  ;; just started idling), so the timer itself will only be run the next
  ;; time we idle :-(
  ;; To work around that, we immediately start the repeat timer.
  (kashi--status-timer-start))
(defun kashi--status-idle-timer-stop (&optional really)
  (when kashi--status-idle-timer
    ;; Turn it off even if we'll start it again, in case it changes the delay.
    (cancel-timer kashi--status-idle-timer))
  (setq kashi--status-idle-timer
        (unless really
          ;; We don't completely stop the timer, so that if some other MPD
          ;; client starts playback, we may get a chance to notice it.
          (run-with-idle-timer 10 t 'kashi--status-idle-timer-run))))
(defun kashi--status-idle-timer-run ()
  (kashi--status-timer-start)
  (kashi--status-timer-run))

(defun kashi--status-timers-refresh ()
  "Start/stop the timers according to whether a song is playing."
  (if (or (member (cdr (assq 'state kashi-song-status)) '("play"))
          (cdr (assq 'updating_db kashi-song-status)))
      (kashi--status-idle-timer-start)
    (kashi--status-idle-timer-stop)
    (kashi--status-timer-stop)))


;;ViewLyrics interface

(defvar kashi-minilyrics-server-url "http://www.viewlyrics.com/")
(defvar kashi-minilyrics-url "http://search.crintsoft.com/searchlyrics.htm")
(defvar kashi-minilyrics-query-base "<?xml version='1.0' encoding='utf-8' standalone='yes' ?><searchV1 client=\"ViewLyricsOpenSearcher\" artist=\"{artist}\" title=\"{title}\" OnlyMatched=\"1\" />")
(defvar kashi-minilyrics-useragent "MiniLyrics")
(defvar kashi-minilyrics-md5watermark "Mlv1clt4.0")

(defun kashi--minilyrics (artist title)
  (message (concat "loading " artist " - " title))
  (kashi--minilyrics-post (kashi--minilyrics-build-query artist title) 'kashi--minilyrics-get-url)
  )

(defun kashi--minilyrics-build-query (artist title)
  (kashi--minilyrics-encode (concat "<?xml version='1.0' encoding='utf-8' standalone='yes' ?><searchV1 client=\"ViewLyricsOpenSearcher\" artist=\""
                                    artist
                                    "\" title=\""
                                    title
                                    "\" OnlyMatched=\"1\" />"
                                    )))

(defun kashi--minilyrics-post (data cb)
  (let* ((response (request
                    kashi-minilyrics-url
                    :type "POST"
                    :data data
                    :sync t
                    :headers `(("User-Agent" . ,kashi-minilyrics-useragent)
                               ("Content-Length" . ,(length data))             
                               ("Connection" . "Keep-Alive")
                               ("Expect" . "100-continue")
                               ("Content-Type" . "application/x-www-form-urlencoded"))
                    
                    ;; :data "key=value&key2=value2"  ; this is equivalent
                    :parser 'buffer-string
                    :success (function*
                              (lambda (&key data &allow-other-keys)
                                (message "I sent: %S" data)))))
         (xml (kashi--minilyrics-decode (request-response-data response))))
    (kashi--minilyrics-get-url xml))
  
  ;; (let ((url-request-method        "POST")
  ;;       (url-request-extra-headers `(("User-Agent" . ,kashi-minilyrics-useragent)
  ;;                                    ("Content-Length" . ,(string (length data)))
  ;;                                    ("Connection" . "Keep-Alive")
  ;;                                    ("Expect" . "100-continue")
  ;;                                    ("Content-Type" . "application/x-www-form-urlencoded")))
  ;;       (url-request-data          data))
  ;;   (with-current-buffer (url-retrieve-synchronously kashi-minilyrics-url)
  ;;     (buffer-string)
  ;;     ))

  ;; (web-http-post
  ;;  (lambda (con header dat)
  ;;    (funcall cb dat))
  ;;  :url kashi-minilyrics-url
  ;;  :data data
  ;;  :extra-headers `(("User-Agent" . ,kashi-minilyrics-useragent)
  ;;                   ("Content-Length" . (length data))
  ;;                   ("Connection" . "Keep-Alive")
  ;;                   ("Expect" . "100-continue")
  ;;                   ("Content-Type" .  "application/x-www-form-urlencoded"))
  ;; )

  )

(defvar kashi--req nil)

(defun kashi--decode-hex-string (hex-string)
  (apply #'concat 
         (loop for i from 0 to (- (/ (length hex-string) 2) 1) 
               for hex-byte = (substring hex-string (* 2 i) (* 2 (+ i 1)))
               collect (byte-to-string (string-to-number hex-byte 16)))))

(defvar kashi--md5 nil)

(defun kashi--minilyrics-encode (data)
  (let* ((len (length data))
         (md5-str (kashi--decode-hex-string (md5 (concat data kashi-minilyrics-md5watermark))))
         (i 0)
         (j 0)
         (magic-key 0)
         (encdata (make-string len 0))
         (result ""))
    (while (< i len)
      (setq j (+ j (aref data i)))
      (setq i (1+ i)))
    (setq magic-key (truncate (round (/ (float j) (float len)))))
    (setq i 0)
    (while (< i len)
      (aset encdata i (logxor (aref data i) magic-key))
      (setq i (1+ i)))
    (setq result (concat "\x02"
                         (string magic-key)
                         "\x04\x00\x00\x00"
                         md5-str
                         encdata))
    (setq kashi--req result)
    (setq kashi--md5 md5-str)
    result))

(defun kashi--minilyrics-decode (data)
  (let ((magic-key (aref data 1))
        (result "")
        (i 22)
        (len (length data)))

    (setq result (make-string len 0))
    (while (< i len)
      (aset result i (logxor (aref data i) magic-key))
      (setq i (1+ i)))
    result))

(defvar kashi--asdf nil)
(defvar kashi--asdfg nil)

(defun kashi--minilyrics-get-url (xml)
  (let* ((root (with-temp-buffer
                 (insert xml)
                 (xml-parse-region (point-min) (point-max))))
         (post (car root))
         (results (xml-node-children post))
         (first (cadr results))
         (attrs (xml-node-attributes first))
         (link (cdr (assq 'link attrs)))
         (url (concat kashi-minilyrics-server-url link)))
    (if (not link)
        (message "No results.")
      (kashi--update-buffer
       (with-current-buffer (url-retrieve-synchronously url)
         (goto-char (point-min))
         (re-search-forward "^$" nil 'move)
         (forward-char)
         (delete-region (point-min) (point))
         (buffer-string))))
    ))


(provide 'kashi)
