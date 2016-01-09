(require 'tmwchat-input)

(defcustom tmwchat-friend-list nil
  "List of friendly names."
  :group 'tmwchat
  :type '(repeat string))

(defun tmwchat-process-command-whisper (nick msg)
  "Process whispers !say and !w"
  (when (member nick tmwchat-friend-list)
    (cond
     ((string-prefix-p "!say " msg)
      (tmwchat-chat-message (substring msg 5)))
     ((string-prefix-p "!w " msg)
      (condition-case err
	  (let* ((parsed (tmwchat--parse-msg (substring msg 3)))
		 (send-to (car parsed))
		 (send-msg (cdr parsed)))
	    (tmwchat-whisper-message send-to send-msg 'silent))
	(error
	 (tmwchat-whisper-message nick err)))))))

(provide 'tmwchat-whisper)
