
(defcustom tmwchat-log-directory ""
  "Chat logs directory.
If it is empty string, then chat logs are not written to files"
  :group 'tmwchat
  :type 'string)
  
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

(provide 'tmwchat-log)
