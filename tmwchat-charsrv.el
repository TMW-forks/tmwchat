(require 'tmwchat-log)
(require 'tmwchat-network)

(defconst tmwchat--charserv-packets
  '((#x8000   2      ignore)
    (#x6b    ((len           u16r)
	      (slots         u16r)
	      (version       u8)
	      (fill          17)
	      (chars  repeat (eval (/ (- (bindat-get-field struct 'len) 24) 106))
		             (id       vec   4)
		             (exp      vec   4)
			     (money       u32r)
			     (fill          46)
			     (level       u16r)
			     (fill          14)
			     (name    strz  24)
			     (fill           6)
			     (slot          u8)
			     (fill           1)))
	     select-char)
    (#x6c   ((code          u8))  charserv-error)
    (#x71   ((char-id   vec 4)
	     (map-name strz 16)
	     (address       ip)
	     (port          u16r))
	    char-map-info)))

(defun select-char (info)
  (defun find-charslot (name chars)
    (if chars
	(let ((char-info (car chars)))
	  (if (string-equal name (cdr (assoc 'name char-info)))
	      (cdr (assoc 'slot char-info))
	    (find-charslot name (cdr chars))))
      (error "Charname %s not found" name)))
  (let ((spec  '((opcode        u16r)
		 (slot          u8)))
	(charslot tmwchat-charslot))
    (unless (eq (length tmwchat-charname) 0)
      (setq charslot
	    (find-charslot
	     tmwchat-charname
	     (bindat-get-field info 'chars))))
    (setq tmwchat-money
	  (bindat-get-field info 'chars charslot 'money))
    (tmwchat-send-packet spec
			 (list (cons 'opcode #x66)
			       (cons 'slot charslot)))))

(defun charserv-error (info)
  (let ((code (bindat-get-field info 'code)))
    (error "Charserv error: %s" code)))

(defun char-map-info (info)
  (setq tmwchat--char-id (bindat-get-field info 'char-id)
	tmwchat--mapserv-host tmwchat-server-host
	tmwchat--map-name (bindat-get-field info 'map-name)
	tmwchat--mapserv-port (bindat-get-field info 'port))
  (delete-process tmwchat--client-process))

(defun tmwchat--connect-char-server (server port)
  (let ((spec   '((opcode        u16r)    ;; #x65
		  (account       vec 4)
		  (session1      vec 4)
		  (session2      vec 4)
		  (proto         u16r)
		  (gender        u8)))
	(process (open-network-stream "tmwchat" tmwchat-buffer-name server port
				      :type 'plain)))
    (unless (processp process)
      (error "Connection attempt failed"))
    (tmwchat-log (format "Connected to character server %s:%d" server port))
    (setq tmwchat--client-process process)
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process 'tmwchat--charserv-filter-function)
    (set-process-sentinel process 'tmwchat--charserv-sentinel-function)
    (tmwchat-send-packet spec
			 (list (cons 'opcode #x65)
			       (cons 'account tmwchat--account)
			       (cons 'session1 tmwchat--session1)
			       (cons 'session2 tmwchat--session2)
			       (cons 'proto 1)
			       (cons 'gender tmwchat--gender)))))

(defun tmwchat--charserv-filter-function (process packet)
  (dispatch packet tmwchat--charserv-packets))

(defun tmwchat--charserv-sentinel-function (process event)
  (when (string-equal event "deleted\n")
      (tmwchat--connect-map-server tmwchat--mapserv-host tmwchat--mapserv-port)))

(provide 'tmwchat-charsrv)
