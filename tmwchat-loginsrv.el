(require 'tmwchat-log)
(require 'tmwchat-network)

(defconst tmwchat-login-error
  '((0 . "Unregistered ID.")
    (1 . "Wrong password.")
    (2 . "Account expired.")
    (3 . "Rejected from server.")
    (4 . (concat
          "You have been permanently banned from the game. "
          "Please contact the GM team."))
    (5 . "Client too old.")
    (6 . (concat
           "You have been temporarily banned from the game "
           "until ?.\nPlease contact the GM team via the forums."))
    (7 . "Server overpopulated.")
    (9 . "This user name is already taken.")
    (10 . "Wrong name.")
    (11 . "Incorrect email.")
    (99 . "Username permanently erased.")))

(defvar tmwchat--reconnect-timer nil
  "Timer for auto-reconnecting.")

(defun tmwchat-login (server port)
  (let ((process (open-network-stream "tmwchat" tmwchat-buffer-name server port
				      :type 'plain)))
    (unless (processp process)
      (error "Connection attempt failed"))
    (tmwchat-log (format "Connected to login server %s:%d" server port))
    (set-process-coding-system process 'binary 'binary)
    (setq tmwchat--client-process process)
    (set-process-filter process 'tmwchat--loginsrv-filter-function)
    (set-process-sentinel process 'tmwchat--loginsrv-sentinel-function)
    (write-u16 #x7530)))  ;; request_version

(defun tmwchat-logoff ()
  (unless (processp tmwchat--client-process)
    (error "Client is not connected"))
  (delete-process tmwchat--client-process))

(defconst tmwchat--loginsrv-packets
  '((#x7531  ((fill          8))   server-version)
    (#x63    ((len           u16r)
	      (host  strz    (eval (- (bindat-get-field struct 'len) 4))))
	     update-host)
    (#x69    ((len           u16r)
	      (session1      vec 4)
	      (account       vec 4)
	      (session2      vec 4)
	      (old-ip        ip)
	      (last-login    strz 24)
	      (fill          2)
	      (gender        u8)
	      (worlds  repeat  ((eval (/ (- (bindat-get-field struct 'len) 47) 32)))
			     (address       ip)
			     (port          u16r)
			     (name          strz 20)
			     (online-users  u16r)
			     (maintenance   u16r)
			     (new           u16r)))
	     login-data)
    (#x6a    ((code         u8)
	      (date  str    20))
	     login-error)))

(defun server-version (info)
  (let ((spec   '((opcode        u16r)    ;; #x64
		  (client-ver    u32r)
		  (username      strz 24)
		  (password      strz 24)
		  (flags         u8)))   ;; 3
	(password (if (zerop (length tmwchat-password))
		      (read-string "TMW password:")
		    tmwchat-password)))
    (tmwchat-send-packet spec
			 (list (cons 'opcode #x64)
			       (cons 'client-ver 201)
			       (cons 'username tmwchat-username)
			       (cons 'password password)
			       (cons 'flags 3)))))

(defun update-host (info)
  (setq tmwchat--update-host   (bindat-get-field info 'host)))

(defun login-data (info)
  (setq	tmwchat--charserv-host tmwchat-server-host
	tmwchat--charserv-port (bindat-get-field info 'worlds 0 'port)
	tmwchat--account       (bindat-get-field info 'account)
	tmwchat--session1      (bindat-get-field info 'session1)
	tmwchat--session2      (bindat-get-field info 'session2)
        tmwchat--gender        (bindat-get-field info 'gender))
  (delete-process tmwchat--client-process))

(defun login-error (info)
  (error "Login error (%s): %s"
	 (bindat-get-field info 'date)
	 (cdr (assoc (bindat-get-field info 'code)
		     tmwchat-login-error))))

(defun tmwchat--loginsrv-filter-function (process packet)
  (dispatch packet tmwchat--loginsrv-packets))

(defun tmwchat--loginsrv-sentinel-function (process event)
  (if (string-equal event "deleted\n")
      (progn
	(queue-empty tmwchat--outgoing-packets)
	(tmwchat--connect-char-server tmwchat--charserv-host
				      tmwchat--charserv-port))
    (when tmwchat-auto-reconnect-interval
      (tmwchat-reconnect tmwchat-auto-reconnect-interval))))

(defun tmwchat-reconnect (&optional delay-seconds)
  (let ((delay-seconds (or delay-seconds 0)))
    (when (timerp tmwchat--reconnect-timer)
      (cancel-timer tmwchat--reconnect-timer))
    (message "Reconnecting in %d seconds")
    (setq tmwchat--reconnect-timer
	  (run-at-time delay-seconds nil
		       'tmwchat-login tmwchat-server-host
		       tmwchat-server-port))))

(provide 'tmwchat-loginsrv)
