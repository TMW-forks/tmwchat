(require 'bindat)
(require 'cl)
(require 'todochiku)
(require 'tmwchat-speedbar)

;;------------------------------------------------------------------
;; Customizable settings
(defgroup tmwchat nil
  "Parameters for TMW chat client."
  :prefix "tmwchat-"
  :group 'emacs)

(defcustom tmwchat-server-host "server.themanaworld.org"
  "TMW server host"
  :group 'tmwchat
  :type 'string)

(defcustom tmwchat-server-port 6901
  "TMW server port"
  :group 'tmwchat
  :type 'integer)

(defcustom tmwchat-username ""
  "TMW username"
  :group 'tmwchat
  :type 'string)

(defcustom tmwchat-password ""
  "TMW password"
  :group 'tmwchat
  :type 'string)

(defcustom tmwchat-charname ""
  "TMW character name"
  :group 'tmwchat
  :type 'string)

(defcustom tmwchat-charslot 0
  "TMW character slot"
  :group 'tmwchat
  :type 'integer)

(defcustom tmwchat-notify-words nil
  "Notify when these words appear"
  :group 'tmwchat
  :type '(repeat string))

(defcustom tmwchat-root-directory "~/.emacs.d/tmwchat"
  "TMWChat root directory"
  :group 'tmwchat
  :type 'string)

(defcustom tmwchat-debug nil
  "Show debugging messages"
  :group 'tmwchat
  :type 'boolean)

(defcustom tmwchat-sound t
  "Play notification sounds"
  :group 'tmwchat
  :type 'boolean)

(defcustom tmwchat-verbose-emotes t
  "Show emotes in chat log"
  :group 'tmwchat
  :type 'boolean)

(defcustom tmwchat-away-message "*AFK* I am away from keyboard"
  "TMW password"
  :group 'tmwchat
  :type 'string)

(defcustom tmwchat-whispers-to-buffers t
  "Send whispers to separate buffers"
  :group 'tmwchat
  :type 'boolean)

;;------------------------------------------------------------------
(defconst tmwchat-emotes
      '((1 . "Disgust")     (2 . "Surprise")     (3 . "Happy")
	(4 . "Sad")         (5 . "Evil")         (6 . "Wink")
	(7 . "Angel")       (8 . "Blush")        (9 . "Tongue")
	(10 . "Grin")       (11 . "Upset")       (12 . "Perturbed")
	(13 . "Blah")       (101 . "Kitty")      (102 . "xD")
	(103 . "^.^")       (104 . "Heart eye")  (105 . "Gold eye")
	(106 . "Sleepy")    (107 . "u.u")        (108 . "-.-'")
	(109 . "Surprised") (110 . "Dead")       (111 . "Look away")
	(112 . "Sad")       (113 . "Palmhead")   (114 . "Evil")
	(115 . "Angry")     (116 . "Purple Sad") (117 . "Insult Buble")
	(118 . "Heart")     (119 . "Emote")      (120 . "Pumpkin")
	(121 . "Evil")      (122 . "Epic")       (123 . "Bad geek")
	(124 . "Mimi")      (125 . "Alien")      (126 . "Troll")
	(127 . "Metal")     (128 . "Crying")))

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

(defvar tmwchat--beings (make-hash-table :test 'equal))
(make-variable-buffer-local 'tmwchat--beings)
(setq tmwchat--client-process nil)
(setq tmwchat--late-id 0)
(setq tmechat--late-msg "")
(setq tmwchat--away nil)

;;----------------------------------------------------------------------
(defconst tmwchat--u16-spec
  '((opcode        u16r)))

(defun write-u16 (arg)
  (process-send-string
   tmwchat--client-process
   (bindat-pack tmwchat--u16-spec (list (cons 'opcode arg)))))
(make-variable-buffer-local 'write-u16)

;;----------------------------------------------------------------------
(defconst tmwchat--login-request-spec
  '((opcode        u16r)    ;; #x64
    (client-ver    u32r)
    (username      strz 24)
    (password      strz 24)
    (flags         u8)))   ;; 3

(defconst tmwchat--server-version-spec
  '((opcode        u16r)    ;; #x7531
    (b1            u8)
    (b2            u8)
    (b3            u8)
    (b4            u8)
    (options       vec 4)))

(defconst tmwchat--update-host-spec
  '((opcode        u16r)    ;; #x63
    (len           u16r)
    (host          strz (eval (- (bindat-get-field struct 'len) 4)))))

(defconst tmwchat--login-response-spec
  '((opcode        u16r)    ;; #x69
    (len           u16r)
    (session1      vec 4)
    (account       vec 4)
    (session2      vec 4)
    (old-ip        ip)
    (last-login    strz 24)
    (fill          2)
    (gender        u8)
    (num-worlds    (eval (/ (- (bindat-get-field struct 'len) 47) 32)))
    (worlds        repeat (num-worlds)
		   (struct tmwchat--world-info-spec))))

(defconst tmwchat--login-error-spec
  '((opcode        u16r)    ;; #x6a
    (code          u8)
    (date          str 20)))

(defconst tmwchat--world-info-spec
  '((address       ip)
    (port          u16r)
    (name          strz 20)
    (online-users  u16r)
    (maintenance   u16r)
    (new           u16r)))

(defun tmwchat-start-client (server port)
  (let ((process (open-network-stream "tmwchat" "*tmwchat*" server port
				      :type 'plain)))
    (unless (processp process)
      (error "Connection attempt failed"))
    (tmwchat-log (format "Connected to login server %s:%d" server port))
    (set-process-coding-system process 'binary 'binary)
    (setq tmwchat--client-process process)
    (set-process-filter process 'tmwchat--loginsrv-filter-function)
    (set-process-sentinel process 'tmwchat--loginsrv-sentinel-function)
    (write-u16 #x7530)))  ;; request_version

(defun tmwchat-stop-client ()
  (unless (processp tmwchat--client-process)
    (error "Client is not connected"))
  (delete-process tmwchat--client-process))

(defun tmwchat--loginsrv-filter-function (process packet)
  (let ((opcode (bindat-get-field
		 (bindat-unpack tmwchat--u16-spec packet)
		 'opcode)))
    ;; (tmwchat-log (format "opcode: %s" opcode))
    (cond ((= opcode #x7531)
	   (let ((info (bindat-unpack tmwchat--server-version-spec packet)))
	     (setq tmwchat--tmwa-version
		   (logior
		    (ash (bindat-get-field info 'b1) 16)
		    (ash (bindat-get-field info 'b2) 8)
		    (bindat-get-field info 'b3))
	           tmwchat--tmwa-options (bindat-get-field info 'options))
	     (process-send-string
	      process
	      (bindat-pack tmwchat--login-request-spec
			   (list (cons 'opcode #x64)
				 (cons 'client-ver 0)
				 (cons 'username tmwchat-username)
				 (cons 'password tmwchat-password)
				 (cons 'flags 3))))))
	  ((= opcode #x63)
	   (let* ((update-host-info (bindat-unpack tmwchat--update-host-spec packet))
		  (uhi-length (bindat-length tmwchat--update-host-spec update-host-info))
		  (login-response-info (bindat-unpack tmwchat--login-response-spec
						      packet uhi-length)))
	     (setq tmwchat--update-host (bindat-get-field update-host-info 'host)
		   ;; (setq tmwchat--charserv-host (bindat-ip-to-string (bindat-get-field login-response-info 'worlds 0 'address)))
		   tmwchat--charserv-host tmwchat-server-host
		   tmwchat--charserv-port (bindat-get-field login-response-info 'worlds 0 'port)
		   tmwchat--account (bindat-get-field login-response-info 'account)
		   tmwchat--session1 (bindat-get-field login-response-info 'session1)
		   tmwchat--session2 (bindat-get-field login-response-info 'session2))
	     (when tmwchat-debug
	       (tmwchat-log (format "%s\n%s" update-host-info login-response-info)))
	     (delete-process process)))
	  ((= opcode #x6a)
	   (let ((info (bindat-unpack tmwchat--login-error-spec packet)))
	     (error
	      (format "Login error (%s): %s"
		      (bindat-get-field info 'date)
		      (assoc (bindat-get-field info 'code)
			     tmwchat-login-error)))))
	  (t (error "Unknown opcode %s" opcode)))))
      
(defun tmwchat--loginsrv-sentinel-function (process event)
  (when (string-equal event "deleted\n")
      (tmwchat--connect-char-server tmwchat--charserv-host tmwchat--charserv-port)))


;;----------------------------------------------------------------------
(defconst tmwchat--connect-charserv-spec
  '((opcode        u16r)    ;; #x65
    (account       vec 4)
    (session1      vec 4)
    (session2      vec 4)
    (proto         u16r)
    (gender        u8)))

(defconst tmwchat--connect-charserv-response-spec
  '((opcode        u16r)     ;; #x6b
    (len           u16r)
    (slots         u16r)
    (version       u8)
    (fill          17)
    (chars         repeat (eval (/ (- (bindat-get-field struct 'len) 24) 106)))
		   (struct tmwchat--charinfo-spec)))

(defconst tmwchat--charinfo-spec
  '((info vec 106  u8)))

(defconst tmwchat--select-char-spec
  '((opcode        u16r)    ;; #x66
    (slot          u8)))

(defconst tmwchat--connect-charserv-error-spec
  '((opcode        u16r)
    (code          u8)))

(defconst tmwchat--charserv-mapinfo-spec
  '((opcode        u16r)    ;; #x71
    (char-id       vec 4)
    (map-name      strz 16)
    (address       ip)
    (port          u16r)))

(defun tmwchat--connect-char-server (server port)
  (let ((process (open-network-stream "tmwchat" "*tmwchat*" server port
				      :type 'plain)))
    (unless (processp process)
      (error "Connection attempt failed"))
    (tmwchat-log (format "Connected to character server %s:%d" server port))
    (setq tmwchat--client-process process)
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process 'tmwchat--charserv-filter-function)
    (set-process-sentinel process 'tmwchat--charserv-sentinel-function)
    (process-send-string
     process
     (bindat-pack tmwchat--connect-charserv-spec
		   (list (cons 'opcode #x65)
		    (cons 'account tmwchat--account)
		    (cons 'session1 tmwchat--session1)
		    (cons 'session2 tmwchat--session2)
		    (cons 'proto 1)
		    (cons 'gender tmwchat--gender))))))

(defun tmwchat--charserv-filter-function (process packet)
  (let ((opcode (bindat-get-field
		 (bindat-unpack tmwchat--u16-spec packet)
		 'opcode)))
    ;; (tmwchat-log (format "opcode: %s" opcode))
    (cond
     ((and (> (length packet) 4) (= opcode #x8000))
      (let ((info (bindat-unpack tmwchat--connect-charserv-response-spec packet 4)))
	(process-send-string
	 process
	 (bindat-pack tmwchat--select-char-spec
		      (list (cons 'opcode #x66)
			    (cons 'slot tmwchat-charslot))))))
     ((= opcode #x6b)
      (let ((info (bindat-unpack tmwchat--connect-charserv-response-spec packet)))
	(process-send-string
	 process
	 (bindat-pack tmwchat--select-char-spec
		      (list (cons 'opcode #x66)
			    (cons 'slot tmwchat-charslot))))))
     ((= opcode #x6c)
      (let ((info (bindat-unpack tmwchat--connect-charserv-error-spec packet)))
	(error "charserv connect error")))
     ((= opcode #x71)
      (let ((info (bindat-unpack tmwchat--charserv-mapinfo-spec packet)))
	(setq tmwchat--char-id (bindat-get-field info 'char-id)
	      ;; tmwchat--mapserv-host (bindat-ip-to-string (bindat-get-field info 'address))
	      tmwchat--mapserv-host tmwchat-server-host
	      tmwchat--mapserv-port (bindat-get-field info 'port))
	(when tmwchat-debug
	  (tmwchat-log (format "%s" info))))
      (delete-process process)))))

(defun tmwchat--charserv-sentinel-function (process event)
  (when (string-equal event "deleted\n")
      (tmwchat--connect-map-server tmwchat--mapserv-host tmwchat--mapserv-port)))

;;=================================================================================
(defconst tmwchat--connect-mapserv-spec
  '((opcode        u16r)    ;; #x72
    (account       vec 4)
    (char-id       vec 4)
    (session1      vec 4)
    (session2      vec 4)
    (gender        u8)))

(defconst tmwchat--connect-mapserv-response-spec
  '((opcode        u16r)    ;; #x73
    (fill          4)       ;; tick
    (coor    vec   3 u8)
    (fill          2)))

(defconst tmwchat--mapserv-packets
  '((#x08a  27 being-action)
    (#x09c  7  being-change-direction)
    (#x0c3  6  being-change-looks)
    (#x1d7  9  being-change-looks-2)
    (#x08d  ((len        u16r)
	      (id         vec 4)
	      (msg   strz (eval (- (bindat-get-field struct 'len) 8))))
	    being-chat)
    (#x0c0  ((id         vec 4)
	      (emote      u8))
	    being-emotion)
    (#x07b  ((id         vec 4)
	      (speed      u16r)
	      (stun-mode  u16r)
	      (status-effects u16r)
	      (options    u16r)
	      (job        u16r)
	      (fill       44))
	    being-move)
    (#x086  14 being-move-2)
    (#x095  ((id         vec 4)
	      (name  strz 24))
	    being-name-response)
    (#x080  ((id         vec 4)
	      (dead-flag  u8))
	    being-remove)
    (#x148  6  being-resurrect)
    (#x19b  ((id         vec 4)
	      (effect     vec 4))
	    being-self-effect)
    (#x07c 39  being-spawn)
    (#x196  7  being-status-change)
    (#x078  ((id         vec 4)
	      (speed      u16r)
	      (stun-mode  u16r)
	      (status-effects u16r)
	      (options    u16r)
	      (job        u16r)
	      (fill       38))
	    being-visible)
    (#x13c  2 player-arrow-equip)
    (#x13b  2 player-arrow-message)
    (#x13a  2 player-attack-range)
    (#x08e  ((len        u16r)
	     (msg   strz (eval (- (bindat-get-field struct 'len) 4))))
	    player-chat)
    (#x0aa  5 player-equip)
    (#x0a4  ((len        u16r)
	     (fill       (eval (- (bindat-get-field struct 'len) 4))))
	    player-equipment)
    (#x195 100 player-guild-party-info)
    (#x1ee  ((len        u16r)
	     (fill       (eval (- (bindat-get-field struct 'len) 4))))
	    player-inventory)
    (#x1c8 11 player-inventory-use)
    (#x1da  ((id         vec 4)
	      (speed      u16r)
	      (stun-mode  u16r)
	      (status-effects u16r)
	      (options    u16r)
	      (job        u16r)
	      (fill       44))
	    player-move)
    (#x139  14 player-move-to-attack)
    (#x10f  ((len         u16r)
	     (fill        (eval (- (bindat-get-field struct 'len) 4))))
	    player-skills)
    (#x0b0  6  player-stat-update-1)
    (#x0b1  6  player-stat-update-2)
    (#x141  12 player-stat-update-3)
    (#x0bc  4  player-stat-update-4)
    (#x0bd  42 player-stat-update-5)
    (#x0be  3  player-stat-update-6)
    (#x119  11 player-status-change)
    (#x088  1  player-stop)
    (#x0ac  5  player-unequip)
    (#x1d8  ((id        vec 4)
	      (speed      u16r)
	      (stun-mode  u16r)
	      (status-effects u16r)
	      (options    u16r)
	      (job        u16r)
	      (fill       38))
	    player-update-1)
    (#x1d9  ((id        vec 4)
	      (speed      u16r)
	      (stun-mode  u16r)
	      (status-effects u16r)
	      (options    u16r)
	      (job        u16r)
	      (fill       37))
	    player-update-2)
    (#x091 20 player-warp)
    (#x081 ((code         u8))
	   connection-problem)
    (#x09a ((len          u16r)
	    (msg   strz (eval (- (bindat-get-field struct 'len) 4))))
	   gm-chat)
    (#x09e 15 item-dropped)
    (#x0a1 4  item-remove)
    (#x09d 15 item-visible) 
    (#x0fb ((len          u16r)
	    (name   strz  24)
	    (fill         (eval (- (bindat-get-field struct 'len) 28))))
	   party-info)
    (#x0fe 28 party-invited)
    (#x107 8  party-update-coords)
    (#x106 8  party-update-hp)
    (#x1b9 4  skill-cast-cancel)
    (#x13e 22 skill-casting)
    (#x1de 31 skill-damage)
    (#x11a 13 skill-no-damage)
    (#x0e5 24 trade-request)
    (#x097 ((len          u16r)
	    (nick  strz   24)
	    (msg   strz (eval (- (bindat-get-field struct 'len) 28))))
	   whisper)
    (#x098  1 whisper-response)))

(defun tmwchat--connect-map-server (server port)
  (let ((process (open-network-stream "tmwchat" "*tmwchat*" server port
				      :type 'plain)))
    (unless (processp process)
      (error "Connection attempt failed"))
    (tmwchat-log (format "Connected to map server to %s:%d" server port))
    (setq tmwchat--client-process process)
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process 'tmwchat--mapserv-filter-function)
    (set-process-sentinel process 'tmwchat--mapserv-sentinel-function)
    (setq tmwchat--ping-timer (run-at-time 15 15 'tmwchat--ping))
    (setq tmwchat--fetch-online-list-timer (run-at-time 5 30 'tmwchat--online-list))
    (process-send-string
     process
     (bindat-pack tmwchat--connect-mapserv-spec
		  (list (cons 'opcode #x72)
			(cons 'account tmwchat--account)
			(cons 'char-id tmwchat--char-id)
			(cons 'session1 tmwchat--session1)
			(cons 'session2 tmwchat--session2)
			(cons 'gender tmwchat--gender))))))

(defun tmwchat--mapserv-sentinel-function (process event)
  (if (string-equal event "deleted\n")
      (message "Exited successfully")
    (error event))
  (cancel-timer tmwchat--ping-timer)
  (cancel-timer tmwchat--fetch-online-list-timer))

(defun tmwchat--mapserv-filter-function (process packet)
  "The process filter for TMW server"
  ;; (tmwchat-log (concat "recv:" packet))
  (setq tmwchat--last-packet packet)
  (let ((opcode (bindat-get-field
		 (bindat-unpack tmwchat--u16-spec packet)
		 'opcode))
	(plength (length packet)))
    ;; (tmwchat-log (format "opcode: %s" opcode))
    (cond
     ((and (> plength 4) (= opcode #x8000))
      (tmwchat--mapserv-filter-function
       process
       (substring packet 4)))
     ((= opcode #x73)
      (let ((info (bindat-unpack tmwchat--connect-mapserv-response-spec packet)))
	(when tmwchat-debug
	  (tmwchat-log (format "%s" info)))
	(tmwchat-log "Type /help <enter> to get infomation about available commands"))
      (write-u16 #x7d))      ;; map-loaded
     ((assoc opcode tmwchat--mapserv-packets)
      (let ((expected-len)
	    (parsed-data)
	    (spec (nth 1 (assoc opcode tmwchat--mapserv-packets)))
	    (fun (nth 2 (assoc opcode tmwchat--mapserv-packets))))
	(if (numberp spec)
	    (setq expected-len spec)
	  (setq parsed-data (bindat-unpack spec packet 2)
		expected-len (bindat-length spec parsed-data)))
	(unless (numberp spec)
          (when (functionp fun)
	    ;; (tmwchat-log (format "%s %s" fun parsed-data))
	    (funcall fun parsed-data)))
	(when (> plength (+ expected-len 2))
	  (let ((new-packet (substring packet (+ expected-len 2))))
	    ;; (tmwchat-log (format "new packet: %s ..."
	    ;; 			 (bindat-unpack tmwchat--u16-spec new-packet)))
	    (tmwchat--mapserv-filter-function
	     process
	     new-packet))))))))

;;--------------------------------------------------------------------------------
(defun being-chat (info)
  (let ((id (bindat-get-field info 'id))
	(msg (decode-coding-string
	      (bindat-get-field info 'msg)
	      'utf-8)))
    (if (gethash id tmwchat--beings)
	(let ((sender (being-name id))
	      (msg2 (tmwchat--remove-color msg)))
	  (when (tmwchat--notify-filter msg2)
	    (tmwchat--notify sender msg2))
	  (tmwchat-log (format "%s: %s" sender msg2)))
      (progn
	(setq tmwchat--late-id id
	      tmwchat--late-msg msg)
	(add-being id 1)))))

(defun being-emotion (info)
  (let ((emote-repr (cdr (assoc (bindat-get-field info 'emote) tmwchat-emotes)))
	(name (being-name (bindat-get-field info 'id))))
    (when (and name emote-repr tmwchat-verbose-emotes)
      (tmwchat-log (format "%s emotes: (%s)" name emote-repr)))))
    
(defun being-move (info)
  ;; add-being ...
  (let ((id (bindat-get-field info 'id))
	(job (bindat-get-field info 'job)))
    (add-being id job)))

(defun being-name-response (info)
  (let ((id (bindat-get-field info 'id))
	(name (bindat-get-field info 'name)))
    (puthash id name tmwchat--beings)
    (setq tmwchat--speedbar-dirty t)
    ;; (tmwchat-log (format "%s pops out" name))
    (when (equal id tmwchat--late-id)
      (setq tmwchat--late-id nil)
      (let ((msg (tmwchat--remove-color tmwchat--late-msg)))
	(when (tmwchat--notify-filter msg)
	  (tmwchat--notify name msg))
	(tmwchat-log (format "%s: %s" name msg))))))

(defun being-visible (info)
  (let ((id (bindat-get-field info 'id))
	(job (bindat-get-field info 'job)))
    (add-being id job)))

(defun being-remove (info)
  (let ((id (bindat-get-field info 'id))
	(dead-flag (bindat-get-field info 'dead-flag)))
    (unless (= dead-flag 1)
      ;; (when (being-name id)
      ;; 	(tmwchat-log (format "%s is gone" (being-name id))))
      (remhash id tmwchat--beings))))

(defconst tmwchat--being-name-request-spec
  '((opcode      u16r)  ;; #x94
    (id          vec 4)))

(defun being-name (id)
  "return being name by ID"
  (gethash id tmwchat--beings))
(make-variable-buffer-local 'being-name)

(defun vec-less (v1 v2)
  (or (< (elt v1 3) (elt v2 3))
      (and (= (elt v1 3) (elt v2 3))
	   (or (< (elt v1 2) (elt v2 2))
	       (and (= (elt v1 2) (elt v2 2))
		    (or (< (elt v1 1) (elt v2 1))
			(and (= (elt v1 1) (elt v2 1))
			     (< (elt v1 0) (elt v2 0)))))))))
(make-variable-buffer-local 'vec-less)

(defun add-being (id job)
  (when (and (vec-less id [128 119 142 6])  ;; 110'000'000
	     (or (<= job 25)
		 (and (>= job 4001)
		      (<= job 4049))))
      (unless (gethash id tmwchat--beings)
        ;; (tmwchat-log (format "adding being %s job %s" id job))
	;;; actual hash table update is done when we get being-name-response
	(process-send-string
	 tmwchat--client-process
	 (bindat-pack tmwchat--being-name-request-spec
		      (list (cons 'opcode #x94)
			    (cons 'id id)))))))
(make-variable-buffer-local 'add-being)

(defun tmwchat--ping ()
  (let ((process (get-process "tmwchat")))
    (when (processp process)
      (with-current-buffer (process-buffer process)
	(process-send-string
	 process
	 (bindat-pack tmwchat--being-name-request-spec
		      (list (cons 'opcode #x94)
			    (cons 'id tmwchat--char-id))))))))

(setq tmwchat-online-users nil)

(when (or (< emacs-major-version 24)
	  (and (= emacs-major-version 24)
	       (< emacs-minor-version 4)))
  (defun string-suffix-p (str1 str2 &optional ignore-case)
    (let ((begin2 (- (length str2) (length str1)))
	  (end2 (length str2)))
      (when (< begin2 0) (setq begin2 0))
      (eq t (compare-strings str1 nil nil
			     str2 begin2 end2
			     ignore-case)))))

(defun tmwchat--online-list ()
  (defun chomp-end (str)
    (when (string-suffix-p "(GM) " str)
      (setq str (substring str 0 -5)))
    (replace-regexp-in-string (rx (* (any " \t\n")) eos)
			      ""
			      str))
  (defun gen-list (str)
    (let* ((m (string-match "------------------------------" str))
	   (end (+ (match-end 0) 1))
	   (strm (substring str end))
	   (m2 (string-match "\n\n" strm))
	   (start (match-beginning 0))
	   (strmm (substring strm 0 start)))
      (mapcar 'chomp-end (split-string strmm "\n"))))
  (defun callback (status)
    (let ((data (buffer-string)))
      (setq tmwchat-online-users (gen-list data))
      (setq tmwchat--speedbar-dirty t)
      (kill-buffer (current-buffer))))
  (let ((url "http://server.themanaworld.org/online.txt"))
    (url-retrieve url 'callback nil t t)))

(defconst tmwchat--show-emote-spec
  '((opcode      u16r)   ;; #xbf
    (id          u8)))

(defun show-emote (id)
  (tmwchat-log (format "(%s)" (cdr (assoc id tmwchat-emotes))))
  (process-send-string
   tmwchat--client-process
   (bindat-pack tmwchat--show-emote-spec
		(list (cons 'opcode #xbf)
		      (cons 'id id)))))
(make-variable-buffer-local 'show-emote)

(defconst tmwchat--chat-message-spec
  '((opcode      u16r)    ;; #x8c
    (len         u16r)
    (msg  strz   (eval (- (bindat-get-field struct 'len) 4)))))

(defun chat-message (msg)
  (let* ((nmsg (encode-coding-string (format "%s : %s" tmwchat-charname msg) 'utf-8))
	 (nlen (length nmsg)))
    (process-send-string
     tmwchat--client-process
     (bindat-pack tmwchat--chat-message-spec
		  (list (cons 'opcode #x8c)
			(cons 'len (+ nlen 5))
			(cons 'msg nmsg))))))
(make-variable-buffer-local 'chat-message)

(defconst tmwchat--chat-whisper-spec
  '((opcode       u16r)    ;; #x96
    (len          u16r)
    (nick   strz  24)
    (msg    strz  (eval (- (bindat-get-field struct 'len) 28)))))

(defun whisper-message (nick msg)
  (let* ((nmsg (encode-coding-string msg 'utf-8))
	 (nlen (length nmsg)))
    (tmwchat--update-recent-users nick)
    (tmwchat-log (format "PM -> %s : %s" nick msg))
    (process-send-string
     tmwchat--client-process
     (bindat-pack tmwchat--chat-whisper-spec
		  (list (cons 'opcode #x096)
			(cons 'len (+ nlen 29))
			(cons 'nick nick)
			(cons 'msg nmsg))))))
(make-variable-buffer-local 'whisper-message)

(defun player-move (info)
  (let ((id (bindat-get-field info 'id))
	(job (bindat-get-field info 'job)))
    (add-being id job)))

(defun player-update-1 (info)
  (let ((id (bindat-get-field info 'id))
	(job (bindat-get-field info 'job)))
    (add-being id job)))

(defun player-update-2 (info)
  (let ((id (bindat-get-field info 'id))
	(job (bindat-get-field info 'job)))
    (add-being id job)))

(defun connection-problem (info)
  (let ((code (bindat-get-field info 'code)))
    (tmwchat-log (format "Connection problem: %s" code))))

(defun player-chat (info)
  (let ((msg (decode-coding-string (bindat-get-field info 'msg) 'utf-8)))
    (if (string-prefix-p tmwchat-charname msg)
	(tmwchat-log (format "%s" msg))
      (tmwchat-log (format "Server: %s" msg)))))

(defun gm-chat (info)
  (let ((msg (decode-coding-string (bindat-get-field info 'msg) 'utf-8)))
    (tmwchat-log (format "GM: %s" msg))))

(defconst tmwchat--trade-response-spec
  '((opcode       u16r)   ;; #xe7
    (code         u8)))

(defun trade-request (info)
  (process-send-string
   tmwchat--client-process
   (bindat-pack tmwchat--trade-response-spec
		(list (cons 'opcode #xe7)
		      (cons 'code 4)))))  ;; reject

(defun whisper (info)
  (let ((nick (bindat-get-field info 'nick))
	(msg
	 (tmwchat--remove-color
	  (decode-coding-string (bindat-get-field info 'msg) 'utf-8))))
    (tmwchat--update-recent-users nick)
    (tmwchat--notify nick msg)
    (tmwchat-log (format "%s whispers: %s" nick msg))
    (when tmwchat--away
      (whisper-message nick tmwchat-away-message))
    (when tmwchat-whispers-to-buffers
      (tmwchat--whisper-to-buffer nick msg))
    ))

;;====================================================================
(defun tmwchat-show-beings ()
  (maphash (lambda (key value)
	     (tmwchat-log (format "%s (id:%s)" value key)))
	   tmwchat--beings))

(defun tmwchat--notify-filter (msg)
  (let ((regex (mapconcat
		'identity
		(mapcar 'regexp-quote tmwchat-notify-words)
		"\\|")))
    (not (eq (string-match-p regex msg) nil))))

(defun tmwchat--notify (title text)
  ;; (message (format "frame-visible: %s" (frame-visible-p (selected-frame))))
  (unless (and (eq (frame-visible-p (selected-frame)) t)
	       (get-buffer-window (process-buffer
				   (get-process "tmwchat"))
				  'visible))
    (let ((icon (concat (file-name-as-directory tmwchat-root-directory)
			"Tmw_logo.png"))
	  (sound (concat (file-name-as-directory tmwchat-root-directory)
			 "newmessage.wav")))
      (todochiku-message title text icon)
      (when tmwchat-sound
	(play-sound-file sound)))))

(defun tmwchat--remove-color (str)
  (while (string-match "##[0-9]" str)
    (setq str (replace-match "" nil nil str)))
  str)

(defun tmwchat--replace-whisper-cmd (nick)
  (interactive "sNick:")
  (defun insert-formatted (nick-q msg)
    (with-current-buffer "*tmwchat*"
      (delete-region tmwchat--start-point (point-max))
      (insert (concat "/w " nick-q " " msg))
      (goto-char (point-max))))
  (with-current-buffer "*tmwchat*"
    (let ((line (buffer-substring tmwchat--start-point (point-max)))
	  (nick-q (if (string-match-p " " nick)
		      (concat "\"" nick "\"")
		    nick)))
      (condition-case nil
	  (if (string-prefix-p "/w " line)
	      (let* ((parsed (tmwchat--parse-msg (substring line 3)))
		     (old-nick (car parsed))
		     (msg (cdr parsed)))
		(unless (string-equal old-nick nick)
		  (insert-formatted nick-q msg)))
	    (insert-formatted nick-q ""))
	(error (insert-formatted nick-q ""))))))

(defun tmwchat--whisper-to-buffer (nick msg)
  (with-current-buffer (get-buffer-create (concat "TMW: " nick))
    (goto-char (point-max))
    ;; if not curr_buffer unread[nick]++
    (insert (format "%s : %s" nick msg))
    (newline)))

;;====================================================================
(defvar tmwchat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'tmwchat-read-print)
    (define-key map "\d" 'tmwchat--backspace)
    map))

(defun tmwchat--backspace ()
  (interactive)
  ;; (message "tmwchat--backspace")
  (unless (equal (point) tmwchat--start-point)
    (backward-delete-char 1)))

(define-derived-mode tmwchat-mode text-mode "TMWChat"
  "Major mode for TMW chat."
  (make-tmwchat-variables))

(defun make-tmwchat-variables ()
  (set (make-local-variable 'tmwchat--start-point) (point))
  ;; (set (make-local-variable 'tmwchat--client-process) nil)
  (set (make-local-variable 'tmwchat--tmwa-version) 0)
  (set (make-local-variable 'tmwchat--tmwa-options) nil)
  (set (make-local-variable 'tmwchat--update-host) nil)
  (set (make-local-variable 'tmwchat--charserv-host) nil)
  (set (make-local-variable 'tmwchat--charserv-port) nil)  
  (set (make-local-variable 'tmwchat--account) 0)
  (set (make-local-variable 'tmwchat--session1) 0)
  (set (make-local-variable 'tmwchat--session2) 0)
  (set (make-local-variable 'tmwchat--gender) 1)
  (set (make-local-variable 'tmwchat--mapserv-host) nil)
  (set (make-local-variable 'tmwchat--mapserv-port) nil)  
  (set (make-local-variable 'tmwchat--char-id) 0)  
  ;; (set (make-local-variable 'tmwchat--late-id) nil)
  ;; (set (make-local-variable 'tmwchat--late-msg) "")
  (set (make-local-variable 'tmwchat-sent) nil)
  (set (make-local-variable 'tmwchat--fetch-online-list-timer) nil)
  (set (make-local-variable 'tmwchat--last-whisper-nick) nil)
  (set (make-local-variable 'tmwchat--ping-timer) nil)
  (mapc (lambda (f)
	  (make-variable-buffer-local (nth 2 f)))
	tmwchat--mapserv-packets)
  )

;;;###autoload
(defun tmwchat ()
  "Switch to tmwchat buffer or make new"
  (interactive)
  (switch-to-buffer "*tmwchat*")
  (tmwchat-mode)
  (setq debug-on-error t)
  (setq truncate-lines nil)
  (setq tmwchat--frame (selected-frame))
  (tmwchat-start-client tmwchat-server-host tmwchat-server-port))

(defun tmwchat-read-print ()
  "Top level loop."
  (interactive)
  (unless (equal tmwchat--start-point (point))
    (setq tmwchat-sent (tmwchat-readin))
    (newline)
    ;; (setq tmwchat--start-point (point))
    (tmwchat-process)
    ;; (newline)
    ))

(defun tmwchat-readin ()
  "Read message and return it"
  (goto-char (point-max))
  (buffer-substring tmwchat--start-point (point)))

(defun tmwchat--parse-msg (msg)
  (unless (and (stringp msg) (> (length msg) 0))
    (error "tmwch0at--parse-msg: msg must be non-empty string"))
  (if (string-match "^\"" msg)
      (if (string-match "\"" (substring msg 1))
	  (cons (substring msg 1 (match-end 0))
		(substring msg (+ (match-end 0) 2)))
	(error "Bad string format"))
    (let ((m (string-match " " msg)))
      (cons (substring msg 0 m)
	    (substring msg (+ m 1))))))

(defun tmwchat-process ()
  (cond
   ((string-equal tmwchat-sent "/help")
    (tmwchat-log
     (concat
      "/help -- show this help\n"
      "/room -- show nearby players\n"
      "/emote <number> -- show emote\n"
      "/emotes -- show emote codes\n"
      "/mute -- mute notification sounds\n"
      "/unmute -- play notification sounds\n"
      "/w NickName Message -- send a PM to NickName\n"
      "/w \"NickName With Spaces\" Message -- send PM to NickName\n"
      "/online -- show online players\n"
      "/away [optional afk message] -- away from keyboard\n"
      "/back -- you are back!\n"
      "/debug -- toggle printing debug information\n"
      "Any other command sends a message to the public chat"
      )))
   ((equal tmwchat-sent "/online")
    (tmwchat-log (format "%s" tmwchat-online-users)))
   ((equal tmwchat-sent "/room")
    (tmwchat-show-beings))
   ((string-prefix-p "/emote " tmwchat-sent)
    (show-emote (string-to-int (substring tmwchat-sent 7))))
   ((string-equal "/emotes" tmwchat-sent)
    (tmwchat-log (format "%s" tmwchat-emotes)))
   ((string-equal "/mute" tmwchat-sent)
    (tmwchat-log "Sounds are muted")
    (setq tmwchat-sound nil))
   ((string-equal "/unmute" tmwchat-sent)
    (tmwchat-log "Sounds are played")
    (setq tmwchat-sound t))
   ((string-equal "/back" tmwchat-sent)
    (setq tmwchat--away nil))
   ((string-prefix-p "/away" tmwchat-sent)
    (setq tmwchat--away t)
    (condition-case nil
	(let ((afk-msg (substring tmwchat-sent 6)))
	  (unless (= (length afk-msg) 0)
	    (setq tmwchat-away-message afk-msg)))
      (error nil))
    (tmwchat-log tmwchat-away-message))
   ((string-equal "/debug" tmwchat-sent)
    (setq tmechat-debug (not tmwchat-debug)))
   ((string-prefix-p "/w " tmwchat-sent)
    (let* ((parsed (tmwchat--parse-msg (substring tmwchat-sent 3)))
	   (nick (car parsed))
	   (msg (cdr parsed)))
      (setq tmwchat--last-whisper-nick nick)
      (whisper-message nick msg)))
   ((string-prefix-p "/ " tmwchat-sent)
    (whisper-message
     tmwchat--last-whisper-nick
     (substring tmwchat-sent 2)))
   (t
    (when (processp tmwchat--client-process)
      (setq tmwchat--last-whisper-nick nil)
      (chat-message tmwchat-sent))))
  (delete-region tmwchat--start-point (point-max))
  (setq tmwchat--start-point (point-max))
  (when tmwchat--last-whisper-nick
    (let ((nick-q (if (string-match-p " " tmwchat--last-whisper-nick)
		      (concat "\"" tmwchat--last-whisper-nick "\"")
		    tmwchat--last-whisper-nick)))
      (insert (concat "/w " nick-q " "))
      (goto-char (point-max)))))


(defun tmwchat-log (msg)
  (defun log ()
    (goto-char tmwchat--start-point)
    (insert msg)
    (newline)
    ;; (add-text-properties (point-min)
    ;; 			 (- (point) 1)
    ;; 			 '(read-only t))
    (setq tmwchat--start-point (point)))
    
  (when (processp (get-process "tmwchat"))
    (with-current-buffer (process-buffer (get-process "tmwchat"))
      (if (equal (point) tmwchat--start-point)
	  (log)
	(save-excursion
	  (log))))))

(provide 'tmwchat)
