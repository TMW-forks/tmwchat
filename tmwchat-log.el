
(defcustom tmwchat-log-directory ""
  "Chat logs directory.
If it is empty string, then chat logs are not written to files"
  :group 'tmwchat
  :type 'string)

(defcustom tmwchat-max-log-buffer-size 1000000
  "Max size of log buffer. After it's reached, the beginning
of buffer is cut."
  :group 'tmwchat
  :type 'integer)

(defun tmwchat--debug-log (msg)
  (when tmwchat-debug
    (with-current-buffer (get-buffer-create "TMWChat-debug")
      (tmwchat-cut-buffer tmwchat-max-log-buffer-size)
      (goto-char (point-max))
      (insert msg)
      (newline))))

(defun tmwchat-log-file (nick msg)
  "Log string <msg> to log file corresponding to <nick>"
  (unless (string-equal tmwchat-log-directory "")
    (let* ((msg (if (string-suffix-p "\n" msg) msg
		  (concat msg "\n")))
	   (basedir (or tmwchat-log-directory
			(concat tmwchat-root-directory "/logs")))
	   (dir (concat basedir (format-time-string "/%Y-%m/%d")))
	   (filename (concat dir "/" nick ".txt")))
      (make-directory dir t)
      (setq msg (format "[%s] %s" (tmwchat-time) msg))
      (let ((message-log-max nil))
	(append-to-file msg nil filename)))))


(defun tmwchat-cut-buffer (max-size &optional cut-chars)
  "If the size of buffer is bigger than max-size, remove
cut-chars from the beginning (or slightly less, to preserve lines.
If cut-chars isn't specified, it's max-size / 2"
  (interactive "nSize: ")
  (when (>= (buffer-size) max-size)
    (save-excursion
      (setq cut-chars (or cut-chars (/ max-size 2)))
      (goto-char (+ (point-min) cut-chars))
      (goto-char (line-beginning-position))
      (let ((inhibit-read-only t)
	    (rollback (- (point) (point-min))))
	(delete-region (point-min) (point))
	(when (boundp 'tmwchat--start-point)
	  (incf tmwchat--start-point (- rollback)))))))


(defun tmwchat-log (&rest args)
  (defun log ()
    (let ((msg (apply 'format args))
	  (msg-l)
	  (inhibit-read-only t))
      (setq msg (format "[%s] %s" (tmwchat-time) msg))
      (setq msg-l (tmwchat--search-urls msg))
      (goto-char tmwchat--start-point)
      (insert (car msg-l))
      (mapc (lambda (url-info)
	      (let ((beg (+ tmwchat--start-point (nth 0 url-info)))
		    (end (+ tmwchat--start-point (nth 0 url-info) (nth 1 url-info)))
		    (href (nth 2 url-info)))
		(tmwchat-make-url beg end href)))
	    (cdr msg-l))
      (newline)
      (setq tmwchat--start-point (point))
      (tmwchat-make-read-only)
      ))
    
  (when (processp (get-process "tmwchat"))
    (with-current-buffer (process-buffer (get-process "tmwchat"))
      (tmwchat-cut-buffer tmwchat-max-log-buffer-size)
      (if (equal (point) tmwchat--start-point)
	  (log)
	(save-excursion
	  (log))))))

(provide 'tmwchat-log)
