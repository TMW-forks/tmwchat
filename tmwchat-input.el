(require 'tmwchat-log)
(require 'tmwchat-inventory)

(defun tmwchat-show-beings ()
  (dolist (id tmwchat-nearby-player-ids)
    (let ((name (tmwchat-being-name id)))
      (tmwchat-log (format "%s (id:%s)" name id)))))


(defun tmwchat-print-inventory ()
  (maphash
   (lambda (index cell)
     (tmwchat-log "index=%d id=%d amount=%d" index
		  (car cell) (cadr cell)))
   tmwchat-player-inventory))


(defun tmwchat--replace-whisper-cmd (nick)
  (interactive "sNick:")
  (defun insert-formatted (nick-q msg)
    (with-current-buffer tmwchat-buffer-name
      (delete-region tmwchat--start-point (point-max))
      (insert (concat "/w " nick-q " " msg))
      (goto-char (point-max))))
  (with-current-buffer tmwchat-buffer-name
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

(defun tmwchat--find-nick-completion ()
  (defun completion-list ()
    (union
     (tmwchat-get-online-users)
     (ring-elements tmwchat-recent-users)))
  (defun filter (condp lst)
    (delq nil
	  (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
  (let ((onl (tmwchat-get-online-users))
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
    (error "tmwchat--parse-msg: msg must be non-empty string"))
  (if (string-match "^\"" msg)
      (if (string-match "\"" (substring msg 1))
	  (cons (substring msg 1 (match-end 0))
		(substring msg (+ (match-end 0) 2)))
	(error "Bad string format"))
    (let ((m (string-match " " msg)))
      (cons (substring msg 0 m)
	    (substring msg (+ m 1))))))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
				    (: (* (any " \t\n")) eos)))
			    ""
			    str))

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
      "/inv -- show player inventory\n"
      "/zeny -- show player money\n"
      "/equip ID -- equip item id\n"
      "/block PlayerName  -- block player\n"
      "/dc -- disconnect\n"
      "/debug -- toggle printing debug information\n"
      "Any other command sends a message to the public chat"
      )))
   ((string-equal tmwchat-sent "/online")
    (tmwchat-log (format "%s" (tmwchat-get-online-users))))
   ((string-equal tmwchat-sent "/room")
    (tmwchat-show-beings))
   ((string-equal tmwchat-sent "/sit")
    (tmwchat-sit))
   ((string-equal tmwchat-sent "/stand")
    (tmwchat-stand))
   ((string-equal tmwchat-sent "/inv")
    (tmwchat-print-inventory))
   ((string-equal tmwchat-sent "/zeny")
    (tmwchat-log "You have %d GP." tmwchat-money))
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
    (setq tmwchat-away nil))
   ((string-prefix-p "/away" tmwchat-sent)
    (setq tmwchat-away t)
    (condition-case nil
	(let ((afk-msg (substring tmwchat-sent 6)))
	  (unless (= (length afk-msg) 0)
	    (setq tmwchat-away-message (concat "*AFK*: " afk-msg))))
      (error nil))
    (tmwchat-log tmwchat-away-message))
   ((string-prefix-p "/equip " tmwchat-sent)
    (tmwchat-equip-item (string-to-int (substring tmwchat-sent 7))))
   ((string-prefix-p "/block " tmwchat-sent)
    (let ((nick (chomp (substring tmwchat-sent 7))))
      (when (string-prefix-p "\"" nick)
	(setq nick (substring nick 1 (- (length nick) 1))))
      (when (> (length nick) 0)
	(tmwchat-log "Blocking player \"%s\"" nick)
	(customize-set-value
	 'tmwchat-blocked-players
	 (add-to-list 'tmwchat-blocked-players nick)))))
   ((string-equal "/dc" tmwchat-sent)
    (tmwchat-logoff))
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
   ((string-prefix-p "/" tmwchat-sent)
    (tmwchat-log "Unknown command. Type /help to get available commands"))
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


(provide 'tmwchat-input)
