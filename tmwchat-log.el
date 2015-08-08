
(defcustom tmwchat-log-directory nil
  "Chat logs directory. If it is set to nil, then chat 
logs are not written to files"
  :group 'tmwchat
  :type 'directory)
  
(defun tmwchat-log-file (nick msg)
  "Log string <msg> to log file corresponding to <nick>"
  (unless (eq tmwchat-log-directory nil)
    (let* ((msg (if (string-suffix-p "\n" msg) msg
		  (concat msg "\n")))
	   (basedir (or tmwchat-log-directory
			(concat tmwchat-root-directory "/logs")))
	   (dir (concat basedir (format-time-string "/%Y-%m/%d")))
	   (filename (concat dir "/" nick ".txt")))
      (make-directory dir t)
      (setq msg (format "[%s] %s" (tmwchat-time) msg))
      (let ((message (lambda (&rest args) t)))
	(append-to-file msg nil filename)))))

(provide 'tmwchat-log)
