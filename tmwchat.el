(require 'bindat)
(require 'cl)
(require 'subr-x)
(require 'todochiku)
(require 'tmwchat-network)
(require 'tmwchat-speedbar)
(require 'tmwchat-log)

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
  "TMW password. If it's not set, the user will be asked for it"
  :group 'tmwchat
  :type 'string)

(defcustom tmwchat-charname ""
  "TMW character name"
  :group 'tmwchat
  :type 'string)

(defcustom tmwchat-charslot 0
  "TMW character slot. Ignored when `tmw-charname' was set"
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

(defcustom tmwchat-away-message "*AFK*: I am away from keyboard"
  "TMW AFK message"
  :group 'tmwchat
  :type 'string)

(defcustom tmwchat-whispers-to-buffers t
  "Send whispers to separate buffers"
  :group 'tmwchat
  :type 'boolean)

(defcustom tmwchat-sticky-notifications t
  "Use sticky (permanent) notifications. They won't disapper from the screen"
  :group 'tmwchat
  :type 'integer)

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

(defconst tmwchat-emotes-2
  [":-D" ":-)" ";-)" ":-(" ":-o" ":-|" ":-/" "B-)" ":-D" ":-[" ":-P"
   "*blush*" ":'-(" "*evil grin*" "*weird emote*" "*ninja*" ":-)" "*star*" "*?*" "*!*" "*idea*" "*->*"
   "*heart*" "^_^" ":-)" ";-)" ":-(" ":-O" ":-(" "*mimi*" "*epic*" "*32 teeth*" "*perturbed*"
   ":-P" "*shame*" "*sad*" "*evil*" "0_o" "*ninja*" "*bad geek*" "*star*" "*?*" "*!*" "*bubble*"
   "*look away*" "*in love*" "*disgust*" "*devil*" "*upset*" "xD" "u.u" "x_x" "*facepalm*" "*evvil*" "*angry*"
   "*epic*" "*metal*" "*crying*" "*...*" "*@:=*" "*cat*" "*sleeping*" "-.-'" "*alien*"])

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
(defvar tmwchat--party-members (make-hash-table :test 'equal))
(defvar tmwchat--players (make-hash-table :test 'equal))
(defvar tmwchat--whisper-target nil)
(defvar tmwchat--date nil)
(setq tmwchat--client-process nil)
(setq tmwchat--late-id 0)
(setq tmechat--late-msg "")
(setq tmwchat--away nil)
(setq tmwchat-online-users nil)

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
	tmwchat--session2      (bindat-get-field info 'session2))
  (delete-process tmwchat--client-process))

(defun login-error (info)
  (error "Login error (%s): %s"
	 (bindat-get-field info 'date)
	 (cdr (assoc (bindat-get-field info 'code)
		     tmwchat-login-error))))
  
(defun tmwchat--loginsrv-filter-function (process packet)
  (dispatch packet tmwchat--loginsrv-packets))
      
(defun tmwchat--loginsrv-sentinel-function (process event)
  (when (string-equal event "deleted\n")
      (tmwchat--connect-char-server tmwchat--charserv-host tmwchat--charserv-port)))

;;==================================================================================
(defconst tmwchat--charserv-packets
  '((#x8000   2      ignore)
    (#x6b    ((len           u16r)
	      (slots         u16r)
	      (version       u8)
	      (fill          17)
	      (chars  repeat (eval (/ (- (bindat-get-field struct 'len) 24) 106))
			     (id       vec   4)
			     (fill          54)
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
    (tmwchat-send-packet spec
			 (list (cons 'opcode #x66)
			       (cons 'slot charslot)))))

(defun charserv-error (info)
  (let ((code (bindat-get-field info 'code)))
    (error "Charserv error: %s" code)))

(defun char-map-info (info)
  (setq tmwchat--char-id (bindat-get-field info 'char-id)
	tmwchat--mapserv-host tmwchat-server-host
	tmwchat--mapserv-port (bindat-get-field info 'port))
  (delete-process tmwchat--client-process))

(defun tmwchat--connect-char-server (server port)
  (let ((spec   '((opcode        u16r)    ;; #x65
		  (account       vec 4)
		  (session1      vec 4)
		  (session2      vec 4)
		  (proto         u16r)
		  (gender        u8)))
	(process (open-network-stream "tmwchat" "*tmwchat*" server port
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

;;=================================================================================
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
    (#x086  ((id         vec 4)
	     (fill          10))
	     being-move-2)
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
    (#x088  ((id         vec 4)
	     (x           u16r)
	     (y           u16r))
	    player-stop)
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
    (#x020 ((id        vec 4)
	    (addr         ip))
	   ip-response)
    (#x19a 12  pvp-set)
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
	    (members repeat
	     (eval (/ (- (bindat-get-field struct 'len) 28) 46))
	     (id     vec  4)
	     (nick  strz  24)
	     (map   strz  16)
	     (leader      u8)
	     (online      u8)))
	   party-info)
    (#x0fe 28 party-invited)
    (#x107 8  party-update-coords)
    (#x106 8  party-update-hp)
    (#x109 ((len          u16r)
	    (id     vec   4)
	    (msg    strz  (eval (- (bindat-get-field struct 'len) 8))))
	   party-chat)
    (#x1b9 4  skill-cast-cancel)
    (#x13e 22 skill-casting)
    (#x1de 31 skill-damage)
    (#x11a 13 skill-no-damage)
    (#x0e5 24 trade-request)
    (#x097 ((len          u16r)
	    (nick  strz   24)
	    (msg   strz (eval (- (bindat-get-field struct 'len) 28))))
	   whisper)
    (#x098 ((code         u8))
	   whisper-response)
    (#x8000 2   ignore)
    (#x73   ((tick vec 4)
	     (coor vec 3)
	     (fill     2))
	    mapserv-connected)
    (#x7f   4   server-ping)
    ))

(defun tmwchat--connect-map-server (server port)
  (let ((spec '((opcode        u16r)    ;; #x72
		(account       vec 4)
		(char-id       vec 4)
		(session1      vec 4)
		(session2      vec 4)
		(gender        u8)))
	(process (open-network-stream "tmwchat" "*tmwchat*" server port
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
    (tmwchat-send-packet spec
			 (list (cons 'opcode #x72)
			       (cons 'account tmwchat--account)
			       (cons 'char-id tmwchat--char-id)
			       (cons 'session1 tmwchat--session1)
			       (cons 'session2 tmwchat--session2)
			       (cons 'gender tmwchat--gender)))))

(defun tmwchat--mapserv-sentinel-function (process event)
  (if (string-equal event "deleted\n")
      (message "Exited successfully")
    (error event))
  (tmwchat--cleanup))

(defun tmwchat--cleanup ()
  (when (timerp tmwchat--ping-timer)
    (cancel-timer tmwchat--ping-timer))
  (when (timerp tmwchat--fetch-online-list-timer)
    (cancel-timer tmwchat--fetch-online-list-timer))
  (when (processp tmwchat--client-process)
    (delete-process tmwchat--client-process))
  (clrhash tmwchat--beings)
  (clrhash tmwchat--party-members))

(defun tmwchat--mapserv-filter-function (process packet)
  (dispatch packet tmwchat--mapserv-packets))

;;--------------------------------------------------------------------------------
(defun being-chat (info)
  (let ((id (bindat-get-field info 'id))
	(msg (bindat-get-field info 'msg)))
    (unless (tmwchat--contains-302-202 msg)
      (setq msg (decode-coding-string msg 'utf-8))
      (if (gethash id tmwchat--beings)
	  (let ((sender (being-name id))
		(msg2 (tmwchat--remove-color msg)))
	    (when (tmwchat--notify-filter msg2)
	      (tmwchat--notify sender msg2))
	    (tmwchat-log (format "%s: %s" sender msg2))
	    (tmwchat-log-file "#General" (format "%s: %s" sender msg2)))
	(progn
	  (setq tmwchat--late-id id
		tmwchat--late-msg msg)
	  (add-being id 1))))))

(defun being-emotion (info)
  (let ((emote-repr (cdr (assoc (bindat-get-field info 'emote) tmwchat-emotes)))
	(name (being-name (bindat-get-field info 'id))))
    (when (and name emote-repr tmwchat-verbose-emotes)
      (tmwchat-log (format "%s emotes: (%s)" name emote-repr)))))
    
(defun being-move (info)
  (let ((id (bindat-get-field info 'id))
	(job (bindat-get-field info 'job)))
    (add-being id job)))

(defun being-move-2 (info)
  (let ((id (bindat-get-field info 'id))
	(job 1))
    (add-being id job)))

(defun being-name-response (info)
  (let ((id (bindat-get-field info 'id))
	(name (bindat-get-field info 'name)))
    (puthash id name tmwchat--beings)
    (setq tmwchat--speedbar-dirty t)
    (when (equal id tmwchat--late-id)
      (setq tmwchat--late-id nil)
      (let ((msg (tmwchat--remove-color tmwchat--late-msg)))
	(when (tmwchat--notify-filter msg)
	  (tmwchat--notify name msg))
	(tmwchat-log (format "%s: %s" name msg))
	(tmwchat-log-file name (format "%s : %s" name msg))
	))))

(defun being-visible (info)
  (let ((id (bindat-get-field info 'id))
	(job (bindat-get-field info 'job)))
    (add-being id job)))

(defun being-remove (info)
  (let ((id (bindat-get-field info 'id))
	(dead-flag (bindat-get-field info 'dead-flag)))
    (unless (= dead-flag 1)
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
  (let ((spec '((opcode    u16r)
		(id    vec 4))))
    (when (and (vec-less id [128 119 142 6])  ;; 110'000'000
	       (or (<= job 25)
		   (and (>= job 4001)
			(<= job 4049))))
      (unless (gethash id tmwchat--beings)
        ;; (tmwchat-log (format "adding being %s job %s" id job))
	;;; actual hash table update is done when we get being-name-response
	(tmwchat-send-packet spec
			     (list (cons 'opcode #x94)
				   (cons 'id id)))))))
(make-variable-buffer-local 'add-being)

(defun tmwchat--ping ()
  (let ((spec '((opcode    u16r)
		(tick  vec 4)))
	(process (get-process "tmwchat")))
    (when (processp process)
      (with-current-buffer (process-buffer process)
	(tmwchat-send-packet spec
			     (list (cons 'opcode #x7e)
				   (cons 'tick   tmwchat--tick)))))))


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
    (let ((onl)
	  (data (buffer-string)))
      (setq onl (gen-list data))
      (setq tmwchat-online-users (copy-sequence onl))
      (setq tmwchat--speedbar-dirty t)
      (kill-buffer (current-buffer))))
  (let ((url "http://server.themanaworld.org/online.txt"))
    (url-retrieve url 'callback nil t t)))

(defun show-emote (id)
  (let ((spec '((opcode    u16r)
		(id        u8))))
    (tmwchat-log "You emote: (%s)" (cdr (assoc id tmwchat-emotes)))
    (tmwchat-send-packet spec
			 (list (cons 'opcode #xbf)
			       (cons 'id id)))))
(make-variable-buffer-local 'show-emote)

(defun chat-message (msg)
  (let* ((spec '((opcode      u16r)    ;; #x8c
		 (len         u16r)
		 (msg  strz   (eval (- (bindat-get-field struct 'len) 4)))))
	 (nmsg (encode-coding-string (format "%s : %s" tmwchat-charname msg) 'utf-8))
	 (nlen (length nmsg)))
    (tmwchat-send-packet spec
			 (list (cons 'opcode #x8c)
			       (cons 'len (+ nlen 5))
			       (cons 'msg nmsg)))))
(make-variable-buffer-local 'chat-message)

(defun whisper-message (nick msg)
  (let* ((spec  '((opcode       u16r)
		  (len          u16r)
		  (nick   strz  24)
		  (msg    strz  (eval (- (bindat-get-field struct 'len) 28)))))
	 (nmsg (encode-coding-string msg 'utf-8))
	 (nlen (length nmsg)))
    (tmwchat--update-recent-users nick)
    (setq msg (tmwchat-escape-percent msg))
    (tmwchat-log "[-> %s] %s" nick msg)
    (tmwchat-log-file nick (format "[-> %s] %s" nick msg))
    (when tmwchat-whispers-to-buffers
      (tmwchat--whisper-to-buffer
       nick
       (format "[%s <-] %s" nick msg)))
    (tmwchat-send-packet spec
			 (list (cons 'opcode #x096)
			       (cons 'len (+ nlen 29))
			       (cons 'nick nick)
			       (cons 'msg nmsg)))))
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
  (let* ((code (bindat-get-field info 'code))
	 (err (if (eq code 2)
		  "Account already in use"
		(format "%s" code))))
    (tmwchat-log "Connection problem: %s" err)))

(defun player-chat (info)
  (let ((msg (bindat-get-field info 'msg)))
    (unless (tmwchat--contains-302-202 msg)
      (setq msg (tmwchat--remove-color
		 (decode-coding-string msg 'utf-8)))
      (if (string-prefix-p tmwchat-charname msg)
	  (progn
	    (tmwchat-log "%s" msg)
	    (tmwchat-log-file "#General" msg))
	(progn
	  (tmwchat--notify "Server" msg)
	  (tmwchat-log "Server: %s" msg)
	  (tmwchat-log-file "#General" (format "Server : %s" msg)))))))

(defun gm-chat (info)
  (let ((msg (bindat-get-field info 'msg)))
    (unless (tmwchat--contains-302-202 msg)
      (setq msg (tmwchat--remove-color
		 (decode-coding-string msg 'utf-8)))
      (tmwchat--notify "GM" msg)
      (tmwchat-log "GM: %s" msg)
      (tmwchat-log-file "#General" (format "GM: %s" msg)))))

(defun trade-request (info)
  (let ((spec   '((opcode       u16r)
		  (code         u8))))
    (tmwchat-send-packet spec
			 (list (cons 'opcode #xe7)
			       (cons 'code 4)))))  ;; reject

(defun whisper (info)
  (let ((nick (bindat-get-field info 'nick))
	(msg (bindat-get-field info 'msg)))
    (unless (tmwchat--contains-302-202 msg)
      (setq msg (tmwchat--remove-color
		 (decode-coding-string msg 'utf-8)))
      (unless (string-prefix-p "!selllist" msg)
	(tmwchat--update-recent-users nick)
	(unless (string-equal nick "guild")
	  (tmwchat--notify nick msg))
	(tmwchat-log (format "[%s ->] %s" nick msg))
	(tmwchat-log-file nick (format "[%s ->] %s" nick msg))
	(when (and tmwchat--away
		   (not (string-equal nick "guild")))
	  (whisper-message nick tmwchat-away-message))
	(when tmwchat-whispers-to-buffers
	  (tmwchat--whisper-to-buffer nick (format "[%s ->] %s" nick msg)))))))

(defun whisper-response (info)
  (let ((code (bindat-get-field info 'code)))
    (when (eq code 1)
      (tmwchat-log "Recepient is offline"))))

(defun party-info (info)
  (let* ((len (bindat-get-field info 'len))
	 (name (bindat-get-field info 'name))
	 (count (/ (- len 28) 46))
	 (curr 0))
    (while (< curr count)
      (let ((member-info)
	    (id     (bindat-get-field info 'members curr 'id))
	    (nick   (bindat-get-field info 'members curr 'nick))
	    (map    (bindat-get-field info 'members curr 'map))
	    (leader (bindat-get-field info 'members curr 'leader))
	    (online (bindat-get-field info 'members curr 'online)))
	(setq member-info (list nick map leader online))
	(puthash id member-info tmwchat--party-members)
	;; (message "%s (%s/%s): %s" name curr count member-info) 
	(setq curr (1+ curr))))))

(defun party-chat (info)
  (let ((id     (bindat-get-field info 'id))
	(msg    (bindat-get-field info 'msg))
	(nick))
    (setq nick (or (ignore-errors
		     (car (gethash id tmwchat--party-members)))
		   (format "%s" id)))
    (setq msg (tmwchat--remove-color msg))
    (tmwchat-log "[Party] %s : %s" nick msg)
    (tmwchat-log-file "#Party" (format "%s : %s" nick msg))
    ))

(defun mapserv-connected (info)
  (let ((tick (bindat-get-field info 'tick))
	(coor (bindat-get-field info 'coor)))
    (when tmwchat-debug
      (tmwchat-log "mapserv-connected  tick=%s coor=%s"
		   tick coor))
    (tmwchat-log "Type /help <enter> to get infomation about available commands")
    (write-u16 #x7d)))  ;; map-loaded

(defun tmwchat-time ()
  (let ((date (format-time-string "%D")))
    (if (string-equal date tmwchat--date)
	(format-time-string "%R")
      (progn
	(setq tmwchat--date date)
	(format-time-string "%D %R")))))

;;-------------------------------------------------------------------
(defconst tmwchat--change-act-spec
  '((opcode    u16r)  ;; #x89
    (fill      4)
    (action    u8)))

(defun tmwchat--sit ()
  (tmwchat-send-packet tmwchat--change-act-spec
		       '((opcode . #x89)
			 (action . 2))))

(defun tmwchat--stand ()
  (tmwchat-send-packet tmwchat--change-act-spec
		       '((opcode . #x89)
			 (action . 3))))

;;-------------------------------------------------------------------
(defconst tmwchat--directions
  '(("down"  .  1)
    ("left"  .  2)
    ("up"    .  4)
    ("right" .  8)))

(defconst tmwchat--being-change-dir-spec
  '((opcode    u16r)  ;; #x9b
    (fill      2)
    (dir       u8)))

(defun tmwchat-turn (direction)
  (let ((dir (cdr (assoc direction tmwchat--directions))))
    (if dir
	(tmwchat-send-packet
	 tmwchat--being-change-dir-spec
	 (list (cons 'opcode #x9b)
	       (cons 'dir dir)))
      (message "Wrong direction: %s" direction))))

(defconst tmwchat--party-message-spec
  '((opcode      u16r)  ; #x0108
    (len         u16r)
    (msg    str  (eval (- (bindat-get-field struct 'len) 4)))))

(defun tmwchat-send-party-message (msg)
  (let* ((nmsg (encode-coding-string msg 'utf-8))
	 (nlen (length nmsg)))
    (tmwchat-send-packet tmwchat--party-message-spec
			 (list (cons 'opcode #x0108)
			       (cons 'len (+ nlen 4))
			       (cons 'msg nmsg)))))

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
      (todochiku-message title text icon tmwchat-sticky-notifications)
      (when tmwchat-sound
	(play-sound-file sound)))))

(defun tmwchat-escape-percent (str)
  (let ((splt (split-string str "%")))
    (mapconcat 'identity splt "%%")))

(defun tmwchat--remove-color (str)
  (while (string-match "##[0-9bB]" str)
    (setq str (replace-match "" nil nil str)))
  (ignore-errors
    (let ((beg) (code) (emote))
      (while (setq beg (string-match "%%[^%]" str))
	(setq code (- (elt str (+ beg 2)) 48)
	      emote (elt tmwchat-emotes-2 code)
	      str (replace-match emote nil nil str)))))
  (setq str (tmwchat-escape-percent str))
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
    (tmwchat-mode)
    (setq-local tmwchat--whisper-target nick)
    (goto-char (point-max))
    ;; if not curr_buffer unread[nick]++
    (insert msg)
    (newline)))

;;----------------------------------------------------------------------
(defun tmwchat-insert-url (href label)
  "Insert a clickable URL at current position"
  (interactive "sUrl:\nsLabel:")
  (insert-button
   label
   'action `(lambda (x) (browse-url ,href))
   'follow-link t
   'help-echo href))

(defun tmwchat-make-url (beg end href)
  "Make a clickable text between positions (beg, end)"
  (interactive "nBeginning\nnEnd:\nshref:")
  (make-button
   beg end
   'action `(lambda (x) (browse-url ,href))
   'follow-link t
   'help-echo href))   

(defun tmwchat--search-urls (str)
  (interactive "sString:")
  (let ((urls-found)
	(href) (label)
	(regex "@@\\([^|]+\\)|\\([^@]+\\)@@"))
    (while (string-match regex str)
      (setq href (substring str (match-beginning 1) (match-end 1))
	    label (substring str (match-beginning 2) (match-end 2)))
      (setq str (replace-match label t t str))
      (setq urls-found (cons (list (match-beginning 0) (length label) href)
			     urls-found)))
    (cons str urls-found)))

(defun tmwchat--make-urls (str)
  (interactive "sURL:")
  (let ((str (concat " " str))
	(schema) (link) (url) (m+url)
 	(regex "[^@|]\\(\\(http\\|https\\|ftp\\)://\\([^\t ]+\\)\\)"))
    (while (string-match regex str)
      (setq schema (substring str (match-beginning 2) (match-end 2))
	    link (substring str (match-beginning 3) (match-end 3))
	    url (concat schema "://" link)
	    m+url (format "[@@%s|%s@@]" url link)
	    str (replace-match m+url t t str 1)))
    (substring str 1)))

(defun tmwchat--contains-302-202 (str)
  "Check if string contains ManaPlus-specific messages with \302\202
   that breaks utf8 decoding"
   (string-match-p ": #o302#o202" str))

;;----------------------------------------------------------------------
(defun tmwchat--find-nick-completion ()
  (defun completion-list ()
    (union
     tmwchat-online-users
     (ring-elements tmwchat-recent-users)))
  (defun filter (condp lst)
    (delq nil
	  (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
  (let ((onl (completion-list))
	(len 3)
	(partial)
	(nick)
	(memb))
    (while (> (length onl) 1)
      (setq partial (buffer-substring (- (point) len) (point)))
      (setq onl (filter (lambda (nick)
			  (string-match-p (regexp-quote partial) nick))
			onl))
      (when (setq memb (member-ignore-case partial onl))
	(setq onl (list (car memb))))
      (setq len (1+ len)))
    (when onl
      (setq len (- len 1))
      (setq nick (car onl))
      (while (string-match-p (regexp-quote partial) nick)
	(setq len (1+ len))
	(setq partial (buffer-substring (- (point) len) (point))))
      (setq len (- len 1))
      (when (eq (aref partial 1) 32)
	(setq len (- len 1)))
      (cons nick len))))

(defun tmwchat--replace-nick-completion (nick partial-len)
  (let* ((pt-end (point))
	 (pt-begin (- pt-end partial-len))
	 (nick-q (if (string-match-p " " nick)
		     (concat "\"" nick "\" ")
		   (concat nick " "))))
    (when (equal (char-before pt-begin) 34)
      (setq pt-begin (- pt-begin 1)))
    (delete-region pt-begin pt-end)
    (insert nick-q)))
 
(defun tmwchat-tab-complete ()
  (interactive)
  (let ((result (tmwchat--find-nick-completion)))
    (when result
      (tmwchat--replace-nick-completion (car result) (cdr result)))))

;;====================================================================
(defun tmwchat-make-read-only ()
  "Make all the text in the current buffer read-only."
  (add-text-properties (point-min) tmwchat--start-point
		       '(read-only t front-sticky t rear-nonsticky t)))

(defvar tmwchat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'tmwchat-read-print)
    (define-key map "\t" 'tmwchat-tab-complete)
    map))

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
  (set (make-local-variable 'tmwchat--tick) [0 0 0 1])
  (mapc (lambda (f)
	  (make-variable-buffer-local (nth 2 f)))
	tmwchat--mapserv-packets)
  (mapc (lambda (f)
	  (make-variable-buffer-local (nth 2 f)))
	tmwchat--loginsrv-packets)
  (mapc (lambda (f)
	  (make-variable-buffer-local (nth 2 f)))
	tmwchat--charserv-packets)  
  )

;;;###autoload
(defun tmwchat ()
  "Switch to tmwchat buffer or make new"
  (interactive)
  (switch-to-buffer "*tmwchat*")
  (tmwchat-mode)
  (setq debug-on-error t)
  (setq max-lisp-eval-depth 4096)
  (setq truncate-lines nil)
  (setq tmwchat--frame (selected-frame))
  (setq tmwchat--window (selected-window))
  (tmwchat-start-client tmwchat-server-host tmwchat-server-port))

(defun tmwchat-read-print ()
  "Top level loop."
  (interactive)
  (unless (equal tmwchat--start-point (point))
    (setq tmwchat-sent (tmwchat-readin))
    (newline)
    ;; (setq tmwchat--start-point (point))
    (tmwchat-parse-input)
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

(defun tmwchat-parse-input ()
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
      "/party Message -- send message to your party\n"
      "/online -- show online players\n"
      "/away [optional afk message] -- away from keyboard\n"
      "/back -- you are back!\n"
      "/sit -- Sit down\n"
      "/stand -- Stand up\n"
      "/turn left|right|up|down -- turn in given direction\n"
      "/dc -- disconnect\n"
      "/debug -- toggle printing debug information\n"
      "Any other command sends a message to the public chat"
      )))
   ((string-equal tmwchat-sent "/online")
    (tmwchat-log (format "%s" tmwchat-online-users)))
   ((string-equal tmwchat-sent "/room")
    (tmwchat-show-beings))
   ((string-equal tmwchat-sent "/sit")
    (tmwchat--sit))
   ((string-equal tmwchat-sent "/stand")
    (tmwchat--stand))
   ((string-prefix-p "/turn " tmwchat-sent)
    (tmwchat-turn (substring tmwchat-sent 6)))
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
   ((string-prefix-p "/party " tmwchat-sent)
    (tmwchat-send-party-message
     (tmwchat--make-urls (substring tmwchat-sent 7))))
   ((string-equal "/back" tmwchat-sent)
    (setq tmwchat--away nil))
   ((string-prefix-p "/away" tmwchat-sent)
    (setq tmwchat--away t)
    (condition-case nil
	(let ((afk-msg (substring tmwchat-sent 6)))
	  (unless (= (length afk-msg) 0)
	    (setq tmwchat-away-message (concat "*AFK*: " afk-msg))))
      (error nil))
    (tmwchat-log tmwchat-away-message))
   ((string-equal "/dc" tmwchat-sent)
    (tmwchat-stop-client))   
   ((string-equal "/debug" tmwchat-sent)
    (setq tmechat-debug (not tmwchat-debug)))
   ((string-prefix-p "/w " tmwchat-sent)
    (let* ((parsed (tmwchat--parse-msg (substring tmwchat-sent 3)))
	   (nick (car parsed))
	   (msg (tmwchat--make-urls (cdr parsed))))
      (setq tmwchat--last-whisper-nick nick)
      (whisper-message nick msg)))
   ;; ((string-prefix-p "/ " tmwchat-sent)
   ;;  (whisper-message
   ;;   tmwchat--last-whisper-nick
   ;;   (substring tmwchat-sent 2)))
   (t
    (if tmwchat--whisper-target
	(whisper-message tmwchat--whisper-target tmwchat-sent)
      (progn
	(setq tmwchat--last-whisper-nick nil)
	(chat-message (tmwchat--make-urls tmwchat-sent))))))
  (delete-region tmwchat--start-point (point-max))
  ;; (setq tmwchat--start-point (point-max))
  (when tmwchat--last-whisper-nick
    (let ((nick-q (if (string-match-p " " tmwchat--last-whisper-nick)
		      (concat "\"" tmwchat--last-whisper-nick "\"")
		    tmwchat--last-whisper-nick)))
      (insert (concat "/w " nick-q " "))
      (goto-char (point-max)))))


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
      (if (equal (point) tmwchat--start-point)
	  (log)
	(save-excursion
	  (log))))))

(provide 'tmwchat)
