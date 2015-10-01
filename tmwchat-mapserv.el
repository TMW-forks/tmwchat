(require 'tmwchat-log)
(require 'tmwchat-network)
(require 'tmwchat-util)

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
	     (fill         8)
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
	      (fill        8)
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
	     (items      repeat
	       (eval (/ (- (bindat-get-field struct 'len) 4) 20))
	       (index  u16r)
	       (id     u16r)
	       (fill     16)))
	    player-equipment)
    (#x195 100 player-guild-party-info)
    (#x1ee  ((len        u16r)
	     (items      repeat
	       (eval (/ (- (bindat-get-field struct 'len) 4) 18))
	       (index  u16r)
	       (id     u16r)
	       (fill      2)
	       (amount u16r)
	       (fill     10)))
	    player-inventory)
    (#xa0   ((index   u16r)
	     (amount  u16r)
	     (id      u16r)
	     (fill      15))
	    player-inventory-add)
    (#xaf   ((index   u16r)
	     (amount  u16r))
	    player-inventory-add)
    (#x1c8 11 player-inventory-use)
    (#x1da  ((id      vec 4)
	     (fill        8)
	     (job      u16r)
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
    (#x1d8  ((id     vec 4)
	     (fill       8)
	     (job     u16r)
	     (fill      38))
	    player-update-1)
    (#x1d9  ((id    vec 4)
	     (fill      8)
	     (job    u16r)
	     (fill     37))
	    player-update-2)
    (#x091 ((map strz   16)
            (x        u16r)
	    (y        u16r))
	   player-warp)
    (#x020 ((id      vec 4)
	    (addr       ip))
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
    (#x7f   4   server-ping)))

(defun being-chat (info)
  (let* ((id (bindat-get-field info 'id))
	 (sender (tmwchat-being-name id))
	 (msg (bindat-get-field info 'msg)))
    (unless (or (tmwchat--contains-302-202 msg)
		(member sender tmwchat-blocked-players))
      (setq msg (tmwchat-decode-string msg))
      (setq msg (tmwchat--remove-color msg))
      (when (tmwchat--notify-filter msg)
	(tmwchat--notify sender msg))
      (tmwchat-log (format "%s: %s" sender msg))
      (tmwchat-log-file "#General" (format "%s: %s" sender msg)))))

(defun being-emotion (info)
  (let ((emote-repr (cdr (assoc (bindat-get-field info 'emote) tmwchat-emotes)))
	(name (tmwchat-being-name (bindat-get-field info 'id))))
    (when (and name emote-repr tmwchat-verbose-emotes
	       (not (member name tmwchat-blocked-players)))
      (tmwchat-log (format "%s emotes: %s" name emote-repr)))))
    
;; (defun being-move (info)
;;   (let ((id (bindat-get-field info 'id))
;; 	(job (bindat-get-field info 'job)))
;;     (tmwchat-add-being id job)))

;; (defun being-move-2 (info)
;;   (let ((id (bindat-get-field info 'id))
;; 	(job 1))
;;     (tmwchat-add-being id job)))

(defun being-name-response (info)
  (let ((id (bindat-get-field info 'id))
	(name (bindat-get-field info 'name)))
    (puthash id name tmwchat-player-names)
    (setq tmwchat--adding-being-ids
	  (delete id tmwchat--adding-being-ids))
    (tmwchat-redisplay-player-name id name)
    (setq tmwchat--speedbar-dirty t)))

(defun being-visible (info)
  (let ((id (bindat-get-field info 'id))
	(job (bindat-get-field info 'job)))
    (tmwchat-add-being id job)))

(defun being-remove (info)
  (let ((id (bindat-get-field info 'id))
	(dead-flag (bindat-get-field info 'dead-flag)))
    (unless (= dead-flag 1)
      (setq tmwchat-nearby-player-ids
	    (delete id tmwchat-nearby-player-ids)))))

(defun player-move (info)
  (let ((id (bindat-get-field info 'id))
	(job (bindat-get-field info 'job)))
    (tmwchat-add-being id job)))

(defun player-inventory (info)
  (let ((items (bindat-get-field info 'items)))
    (setq tmwchat-player-inventory items)))

(defun player-equipment (info)
  (let ((items (bindat-get-field info 'items)))
    (setq tmwchat-player-equipment items)))

(defun player-update-1 (info)
  (let ((id (bindat-get-field info 'id))
	(job (bindat-get-field info 'job)))
    (tmwchat-add-being id job)))

(defun player-update-2 (info)
  (let ((id (bindat-get-field info 'id))
	(job (bindat-get-field info 'job)))
    (tmwchat-add-being id job)))

(defun connection-problem (info)
  (let* ((code (bindat-get-field info 'code))
	 (err (if (eq code 2)
		  "Account already in use"
		(format "%s" code))))
    (tmwchat-log "Connection problem: %s" err)
    (tmwchat-logoff)))

(defun player-chat (info)
  (let ((msg (bindat-get-field info 'msg)))
    (unless (tmwchat--contains-302-202 msg)
      (setq msg (tmwchat--remove-color
		 (tmwchat-decode-string msg)))
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
		 (tmwchat-decode-string msg)))
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
    (unless (or (tmwchat--contains-302-202 msg)
		(member nick tmwchat-blocked-players))
      (setq msg (tmwchat--remove-color
		 (tmwchat-decode-string msg)))
      (unless (string-prefix-p "!selllist" msg)
	(tmwchat--update-recent-users nick)
	(unless (string-equal nick "guild")
	  (tmwchat--notify nick msg))
	(tmwchat-log (format "[%s ->] %s" nick msg))
	(tmwchat-log-file nick (format "[%s ->] %s" nick msg))
	(when (and tmwchat-away
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
	(puthash id member-info tmwchat-party-members)
	;; (message "%s (%s/%s): %s" name curr count member-info) 
	(setq curr (1+ curr))))))

(defun party-chat (info)
  (let ((id     (bindat-get-field info 'id))
	(msg    (bindat-get-field info 'msg))
	(nick))
    (setq nick (or (ignore-errors
		     (car (gethash id tmwchat-party-members)))
		   (format "{{ID:%s}}" id)))
    (setq msg (tmwchat--remove-color msg))
    (tmwchat-log "[Party] %s : %s" nick msg)
    (tmwchat-log-file "#Party" (format "%s : %s" nick msg))))

(defun player-warp (info)
  (let ((map    (bindat-get-field info 'map))
	(x      (bindat-get-field info 'x))
	(y      (bindat-get-field info 'y)))
    (tmwchat-log "Warped to %s %d,%d" map x y)
    (write-u16 #x7d)))

(defun mapserv-connected (info)
  (let ((tick (bindat-get-field info 'tick))
	(coor (bindat-get-field info 'coor))
	(x) (y))
    (setq coor (tmwchat-read-coordinates coor))
    (when tmwchat-debug
      (tmwchat-log "mapserv-connected  tick=%s map=%s coor=%s"
		   tick tmwchat--map-name coor))
    (tmwchat-log "Type /help <enter> to get infomation about available commands")
    (write-u16 #x7d)))  ;; map-loaded

;;=====================================================================

(defun tmwchat-add-being (id job)
  (when (and (vec-less id [128 119 142 6])  ;; 110'000'000
	     (or (<= job 25)
		 (and (>= job 4001)
		      (<= job 4049))))
    (add-to-list 'tmwchat-nearby-player-ids id)
    (unless (or (gethash id tmwchat-player-names)
		(member id tmwchat--adding-being-ids))
      (let ((spec '((opcode    u16r)
		    (id    vec 4))))
	(push id tmwchat--adding-being-ids)
	(tmwchat-send-packet spec
			     (list (cons 'opcode #x94)
				   (cons 'id id)))))))

(make-variable-buffer-local 'tmwchat-add-being)

(defun tmwchat-ping-mapserv ()
  (let ((spec '((opcode    u16r)
		(tick  vec 4)))
	(process (get-process "tmwchat")))
    (when (processp process)
      (with-current-buffer (process-buffer process)
	(tmwchat-send-packet spec
			     (list (cons 'opcode #x7e)
				   (cons 'tick   tmwchat--tick)))))))

(defun show-emote (id)
  (let ((spec '((opcode    u16r)
		(id        u8))))
    ;; (tmwchat-log "You emote: (%s)" (cdr (assoc id tmwchat-emotes)))
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
       (format "[-> %s] %s" nick msg)))
    (tmwchat-send-packet spec
			 (list (cons 'opcode #x096)
			       (cons 'len (+ nlen 29))
			       (cons 'nick nick)
			       (cons 'msg nmsg)))))
(make-variable-buffer-local 'whisper-message)

;;-------------------------------------------------------------------
(defun tmwchat-equip-item (id)
  (interactive "nItem ID:")
  (defun find-item-index (lst id)
    (cond
     ((equal lst nil) -1)
     ((equal id (cdr (assoc 'id (car lst))))
      (cdr (assoc 'index (car lst))))
     (t (find-item-index (cdr lst) id))))
  (let ((spec '((opcode    u16r)
		(index     u16r)
		(fill      2)))
	(idx (find-item-index tmwchat-player-equipment id)))
    (if (> idx -1)
	(tmwchat-send-packet spec
			     (list (cons 'opcode #xa9)
				   (cons 'index  idx)))
      (tmwchat-log "[error] Cannot find item id %d" id))))

;;-------------------------------------------------------------------
(defun tmwchat-send-party-message (msg)
  (let* ((spec   '((opcode      u16r)  ; #x0108
		   (len         u16r)
		   (msg    str  (eval (- (bindat-get-field struct 'len) 4)))))
	 (nmsg (encode-coding-string msg 'utf-8))
	 (nlen (length nmsg)))
    (tmwchat-send-packet spec
			 (list (cons 'opcode #x0108)
			       (cons 'len (+ nlen 4))
			       (cons 'msg nmsg)))))

;;-------------------------------------------------------------------
(defconst tmwchat--change-act-spec
  '((opcode    u16r)  ;; #x89
    (fill      4)
    (action    u8)))

(defun tmwchat-sit ()
  (tmwchat-log "You sit down")
  (tmwchat-send-packet tmwchat--change-act-spec
		       '((opcode . #x89)
			 (action . 2))))

(defun tmwchat-stand ()
  (tmwchat-log "You stand up")
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
	(progn
	  (tmwchat-log "You turn to %s" direction)
	  (tmwchat-send-packet
	   tmwchat--being-change-dir-spec
	   (list (cons 'opcode #x9b)
		 (cons 'dir dir))))
      (message "Wrong direction: %s" direction))))

;;=====================================================================
(defun tmwchat--connect-map-server (server port)
  (let ((spec '((opcode        u16r)    ;; #x72
		(account       vec 4)
		(char-id       vec 4)
		(session1      vec 4)
		(session2      vec 4)
		(gender        u8)))
	(process (open-network-stream "tmwchat" tmwchat-buffer-name server port
				      :type 'plain)))
    (unless (processp process)
      (error "Connection attempt failed"))
    (tmwchat-log (format "Connected to map server to %s:%d" server port))
    (setq tmwchat--client-process process)
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process 'tmwchat--mapserv-filter-function)
    (set-process-sentinel process 'tmwchat--mapserv-sentinel-function)
    (setq tmwchat-ping-mapserv-timer (run-at-time 15 15 'tmwchat-ping-mapserv))
    (setq tmwchat--random-equip-timer (run-at-time
				       10
				       tmwchat-auto-equip-interval
				       'tmwchat-equip-random-item))
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

(defun tmwchat--mapserv-filter-function (process packet)
  (dispatch packet tmwchat--mapserv-packets))


;;======================================================================
;; TODO: should also update being cache if name not on online list
;; but now online list is buggy, so we skip it
(defun tmwchat-being-name (id)
  "return being name by ID"
  (let ((name (gethash id tmwchat-player-names)))
    (if name
	name
      (let ((name-m (format "{{ID:%s}}" id)))
	(tmwchat-add-being id 1)
	name-m))))



(make-variable-buffer-local 'tmwchat-being-name)


(provide 'tmwchat-mapserv)
