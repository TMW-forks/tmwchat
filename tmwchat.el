(require 'bindat)
(require 'cl)
;; (require 'subr-x)
(require 'todochiku)
(require 'tmwchat-loginsrv)
(require 'tmwchat-charsrv)
(require 'tmwchat-mapserv)
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

(defcustom tmwchat-root-directory "~/tmwchat"
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

(defcustom tmwchat-whispers-to-buffers nil
  "Send whispers to separate buffers. Feature is incomplete"
  :group 'tmwchat
  :type 'boolean)

(defcustom tmwchat-sticky-notifications t
  "Use sticky (permanent) notifications. They won't disapper from the screen"
  :group 'tmwchat
  :type 'integer)

(defcustom tmwchat-auto-equip-item-ids nil
  "List of item IDs to periodically equip"
  :group 'tmwchat
  :type '(repeat integer))

(defcustom tmwchat-auto-equip-interval 15
  "Interval betwee auto-equipping next item from `tmwchat-auto-equip-item-ids' list"
  :group 'tmwchat
  :type '(repeat integer))

(defcustom tmwchat-blocked-players nil
  "Blocked players list"
  :group 'tmwchat
  :type '(repeat string))

;;------------------------------------------------------------------
(defconst tmwchat-emotes
      '((1 . "Disgust")     (2 . "0_o")          (3 . ":-)")
	(4 . ":-(")         (5 . ">:-]")         (6 . ";-)")
	(7 . "O:-)")        (8 . "Blush")        (9 . ":-P")
	(10 . ":-D")        (11 . "Upset")       (12 . "Perturbed")
	(13 . "Blah")       (101 . ":3")         (102 . "xD")
	(103 . "^.^")       (104 . "Heart eye")  (105 . "$_$")
	(106 . "Sleepy")    (107 . "u.u")        (108 . "-.-'")
	(109 . "Surprised") (110 . "Dead")       (111 . ">_>")
	(112 . "Sad")       (113 . "Facepalm")   (114 . "Evil")
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
   ">_>" "*in love*" "*disgust*" "*devil*" "*upset*" "xD" "u.u" "x_x" "*facepalm*" "*evvil*" "*angry*"
   "*epic*" "*metal*" "*crying*" "*...*" "*@:=*" "*cat*" "*sleeping*" "-.-'" "*alien*"])

(defconst tmwchat-buffer-name "[TMWchat]"
  "Name of main TMWChat buffer")

(defvar tmwchat-player-names (make-hash-table :test 'equal)
  "Player names' cache")
(defvar tmwchat-nearby-player-ids nil "IDs of nearby players")

;; (make-variable-buffer-local 'tmwchat-player-names)
(defvar tmwchat-party-members (make-hash-table :test 'equal))
(defvar tmwchat--whisper-target nil)
(defvar tmwchat--date nil)
(defvar tmwchat--fetch-online-list-timer nil
  "Timer for downloading online.txt")
(defvar tmwchat-ping-mapserv-timer nil
  "Timer for sending PING to mapserv")
(defvar tmwchat--random-equip-timer nil
  "Timer for equipping random item")
(defvar tmwchat-player-inventory nil
  "list containing player inventory items")
(defvar tmwchat-player-equipment nil
  "list containing player equipment")
(defvar tmwchat--last-item-equipped 0
  "the ID of last random item that was equipped")

(defvar tmwchat-away nil "User is away if value is t")

(setq tmwchat--client-process nil)

(defvar tmwchat--adding-being-ids nil
  "Set of IDs that are being requested from server")

(setq tmwchat--online-list-0 nil)
(setq tmwchat--online-list-1 nil)
(setq tmwchat--online-list-number 0)

(defvar tmwchat--map-name nil
  "Current map name")


(defun tmwchat--cleanup ()
  (when (timerp tmwchat-ping-mapserv-timer)
    (cancel-timer tmwchat-ping-mapserv-timer))
  (when (timerp tmwchat--fetch-online-list-timer)
    (cancel-timer tmwchat--fetch-online-list-timer))
  (when (timerp tmwchat--random-equip-timer)
    (cancel-timer tmwchat--random-equip-timer))
  (when (processp tmwchat--client-process)
    (delete-process tmwchat--client-process))
  (setq tmwchat-nearby-player-ids nil)
  (clrhash tmwchat-player-names)
  (clrhash tmwchat-party-members))


(defun tmwchat-equip-random-item (&rest items-list)
  (setq items-list (or items-list tmwchat-auto-equip-item-ids))
  (let ((next-id (car items-list))
	(len (length items-list)))
    (cond
     ((= len 0) nil)
     ((= len 1)
      (unless (= next-id tmwchat--last-item-equipped)
	(setq tmwchat--last-item-equipped next-id)
	(tmwchat-equip-item next-id)))
     ((> len 1)
      (while (= tmwchat--last-item-equipped next-id)
	(setq next-id (nth (random len) items-list)))
      (setq tmwchat--last-item-equipped next-id)
      (tmwchat-equip-item next-id)))))





;;-------------------------------------------------------------------




;;====================================================================
(defun tmwchat-show-beings ()
  (dolist (id tmwchat-nearby-player-ids)
    (let ((name (tmwchat-being-name id)))
      (tmwchat-log (format "%s (id:%s)" name id)))))

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
  (string-match-p "^#o302|#o202" str))

;;----------------------------------------------------------------------
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
  (set (make-local-variable 'tmwchat-sent) nil)
  (set (make-local-variable 'tmwchat--last-whisper-nick) nil)
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
  (switch-to-buffer tmwchat-buffer-name)
  (tmwchat-mode)
  (setq debug-on-error t)
  (setq max-lisp-eval-depth 4096)
  (setq truncate-lines nil)
  (setq tmwchat--frame (selected-frame))
  (setq tmwchat--window (selected-window))
  (tmwchat-login tmwchat-server-host tmwchat-server-port))

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

(defun tmwchat-set-online-users (users-list)
  (cond
   ((= tmwchat--online-list-number 0)
    (setq tmwchat--online-list-number 1
	  tmwchat--online-list-0 users-list))
   ((= tmwchat--online-list-number 1)
    (setq tmwchat--online-list-number 0
	  tmwchat--online-list-1 users-list))))

(defun tmwchat-get-online-users ()
  (cond
   ((= tmwchat--online-list-number 0)
    tmwchat--online-list-1)
   ((= tmwchat--online-list-number 1)
    tmwchat--online-list-0)))

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
      (tmwchat-set-online-users onl)
      (setq tmwchat--speedbar-dirty t)
      (kill-buffer (current-buffer))))
  (let ((url "http://server.themanaworld.org/online.txt"))
    (url-retrieve url 'callback nil t t)))

(defun tmwchat-time ()
  (let ((date (format-time-string "%D")))
    (if (string-equal date tmwchat--date)
	(format-time-string "%R")
      (progn
	(setq tmwchat--date date)
	(format-time-string "%D %R")))))

(defun tmwchat-redisplay-player-name (id name)
  (with-current-buffer tmwchat-buffer-name
    (save-excursion
      (let ((inhibit-read-only t)
	    (old-name (format "{{ID:%s}}" id)))
	(replace-string old-name name nil (point-min) (point-max))
	(goto-char (point-max))
	(beginning-of-line)
	(setq tmwchat--start-point (point))))))

(provide 'tmwchat)
