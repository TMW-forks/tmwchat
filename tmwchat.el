(require 'bindat)
(require 'cl)
;; (require 'subr-x)
(require 'todochiku)
(require 'tmwchat-loginsrv)
(require 'tmwchat-charsrv)
(require 'tmwchat-mapserv)
(require 'tmwchat-inventory)
(require 'tmwchat-speedbar)
(require 'tmwchat-input)
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

(defcustom tmwchat-auto-equip-interval nil
  "Interval betwee auto-equipping next item from `tmwchat-auto-equip-item-ids' list.
Set it to nil if you want to disable this feature."
  :group 'tmwchat
  :type '(choice (const :tag "Off" nil)
		 (integer :tag "Seconds")))

(defcustom tmwchat-blocked-players nil
  "Blocked players list"
  :group 'tmwchat
  :type '(repeat string))

(defcustom tmwchat-auto-reconnect-interval nil
  "Interval between attempting auto-reconnect when connection is lost (seconds)."
  :group 'tmwchat
  :type '(choice (const :tag "Off" nil)
		 (integer :tag "Seconds")))

(defcustom tmwchat-after-connect-hook nil
  "List of functions to be executed after the player successfully
logged in and MAP_LOADED packet was sent."
  :group 'tmwchat
  :type 'hook)

(defcustom tmwchat-info-message "[@@https://bitbucket.org/rumly111/tmwchat|TMWchat@@] by Travolta"
  "info message, standard answer to !help or !info whisper."
  :group 'tmwchat
  :type 'string)

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
(defvar tmwchat--last-item-equipped 0
  "the ID of last random item that was equipped")

(defvar tmwchat-away nil "User is away if value is t")
(defvar tmwchat--client-process nil)

(defvar tmwchat--adding-being-ids nil
  "Set of IDs that are being requested from server")

(defconst tmwchat-status-emote #xc0
  "Basic status emote")

(setq tmwchat--online-list-0 nil)
(setq tmwchat--online-list-1 nil)
(setq tmwchat--online-list-number 0)

(defvar tmwchat-coor-x 0 "x coordinate of player")
(defvar tmwchat-coor-y 0 "y coordinate of player")
(defvar tmwchat-map-name "" "current map name")
(defvar tmwchat-money 0 "Money of player")

(defvar tmwchat-itemdb (make-hash-table :test 'equal)
  "Item DB, containing item IDs and names")


;; for emacs v24.3 and below
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

;;===================================================================
(defun tmwchat--cleanup ()
  (when (timerp tmwchat--packet-sending-timer)
    (unless tmwchat--reconnecting
      (cancel-timer tmwchat--packet-sending-timer)))
  (cancel-function-timers 'tmwchat-ping-mapserv)
  (cancel-function-timers 'tmwchat-equip-random-item)
  (cancel-function-timers 'tmwchat--online-list)
  (cancel-function-timers 'tmwchat-show-status)
  (when (processp tmwchat--client-process)
    (delete-process tmwchat--client-process))
  (setq tmwchat-nearby-player-ids nil)
  (queue-clear tmwchat--outgoing-packets)
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

;;=================================================================
(defun tmwchat-show-status ()
  (let ((emote tmwchat-status-emote))
    (when tmwchat-shop-mode
      (setq emote (+ emote 1)))
    (when tmwchat-away
      (setq emote (+ emote 2)))
    (tmwchat-show-emote emote)))

;;=================================================================
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


;;====================================================================
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


;;====================================================================
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
    (save-match-data
      (let ((pos) (n) (lst))
	(string-match "------------------------------" str)
	(setq pos (+ (match-end 0) 1)
	      str (substring str pos))
	(string-match "\n\n" str)
	(setq pos (match-beginning 0)
	      n   (read (substring str (+ pos 2)))
	      str (substring str 0 pos)
	      lst (mapcar 'chomp-end (split-string str "\n")))
	(unless (eq n (length lst))
	  (error "Length of online list should be %d" n))
	lst)))

  (defun callback (status)
    (let ((onl)
	  (data (buffer-string)))
      (setq onl (gen-list data))
      (tmwchat-set-online-users onl)
      (tmwchat-update-nearby-player-names onl)
      (setq tmwchat--speedbar-dirty t)
      (kill-buffer (current-buffer))))

  (let ((url "http://server.themanaworld.org/online.txt"))
    (with-demoted-errors "url-retieve: %S"
      (url-retrieve url 'callback nil t t))))

(defun tmwchat-update-nearby-player-names (online-list)
  (dolist (id tmwchat-nearby-player-ids)
    (let ((name (gethash id tmwchat-player-names)))
      (when name
	(unless (member name online-list)
	  (remhash id tmwchat-player-names)
	  (tmwchat-add-being id 1))))))

;;====================================================================
(defun tmwchat-time ()
  (let ((date (format-time-string "%D")))
    (if (string-equal date tmwchat--date)
	(format-time-string "%R")
      (progn
	(setq tmwchat--date date)
	(format-time-string "%D %R")))))

(defun tmwchat--whisper-to-buffer (nick msg)
  (with-current-buffer (get-buffer-create (concat "TMW: " nick))
    (tmwchat-mode)
    (setq-local tmwchat--whisper-target nick)
    (goto-char (point-max))
    ;; if not curr_buffer unread[nick]++
    (insert msg)
    (newline)))

;;====================================================================
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
  (setq truncate-lines nil)
  (setq tmwchat--frame (selected-frame))
  (setq tmwchat--window (selected-window))
  (tmwchat-read-itemdb
   (concat tmwchat-root-directory "/itemdb.txt")
   tmwchat-itemdb)
  (tmwchat-login tmwchat-server-host tmwchat-server-port))



(provide 'tmwchat)
