(require 'tmwchat-inventory)

(defcustom tmwchat-trade-selling nil
  "List of selling items."
  :group 'tmwchat
  :type '(repeat (list :tag "Selling"
		       (integer :tag "ID")
		       (integer :tag "Price"))))

(defcustom tmwchat-trade-buying nil
  "List of item IDs you want to buy."
  :group 'tmwchat
  :type '(repeat (list :tag "Buying"
		       (integer :tag "ID")
		       (integer :tag "Price")
		       (integer :tag "Max amount"))))

(defcustom tmwchat-shop-mode nil
  "Shop mode."
  :group 'tmwchat
  :type 'boolean)

(defcustom tmwchat-after-trade-hook nil
  "List of functions to call after successful trade.
Each function receives 2 hashtables: items gave and received."
  :group 'tmwchat
  :options '(tmwchat-show-successful-trade tmwchat-trade-notify-admins)
  :type 'hook)

(defcustom tmwchat-shop-admins nil
  "List of players who can freely add items to shop"
  :group 'tmwchat
  :type '(repeat string))

(defcustom tmwchat-shop-gift-ids nil
  "List of IDs of small gifts (candies, acorns...) that shop
adds to trade when you buy something."
  :group 'tmwchat
  :type '(repeat integer))

(defvar tmwchat--trade-player ""
  "Player you currently trade with")
(defvar tmwchat--trade-item-id 0)
(defvar tmwchat--trade-item-amount 0)
(defvar tmwchat--trade-item-price 0)
(defvar tmwchat--trade-player-offer 0)
(defvar tmwchat--trade-player-item-id 0)
(defvar tmwchat--trade-player-item-amount 0)
(defvar tmwchat--trade-player-should-pay 0)
(defvar tmwchat--trade-shop-should-pay 0)
;;(defvar tmwchat--trade-cancel-timer nil
;;  "Timer to cancel trade if player hesitates too long")
(defvar tmwchat--trade-mode nil
  "Trade mode (nil, 'buy, 'sell)")
(defvar tmwchat--trade-give-ids (make-hash-table :test 'equal))
(defvar tmwchat--trade-receive-ids (make-hash-table :test 'equal))

(defun tmwchat-encode-base94 (value size)
  (let ((output "")
	(base 94)
	(start 33))
    (while (> value 0)
      (setq output (concat output (string (+ (% value base) start)))
	    value (/ value base)))
    (while (< (length output) size)
      (setq output (concat output (string start))))
    output))

(defun tmwchat-selllist ()
  (defun r ()
    (+ 33 (random 61)))

  (let ((data "\302\202B1"))
    (dolist (item tmwchat-trade-selling)
      (let* ((id (car item))
	     (price (cadr item))
	     (inv-amount (tmwchat-inventory-item-amount id)))
	(when (> inv-amount 0)
	  (setq data (concat
		      data
		      (tmwchat-encode-base94 id 2)
		      (tmwchat-encode-base94 price 4)
		      (tmwchat-encode-base94 inv-amount 3))))))
    (concat data (string (r) (r)))))

(defun tmwchat-buylist ()
  (defun r ()
    (+ 33 (random 61)))

  (let ((data "\302\202S1"))
    (dolist (item tmwchat-trade-buying)
      (let* ((id (car item))
	     (price (cadr item))
	     (max-amount (caddr item))
	     (inv-amount (tmwchat-inventory-item-amount id))
	     (can-afford-amount
	      (min (- max-amount inv-amount)
		   (/ tmwchat-money price))))
	(when (> can-afford-amount 0)
	  (setq data (concat
		      data
		      (tmwchat-encode-base94 id 2)
		      (tmwchat-encode-base94 price 4)
		      (tmwchat-encode-base94 can-afford-amount 3))))))
    (concat data (string (r) (r)))))


(defun tmwchat-invlist ()
  (defun r ()
    (+ 33 (random 61)))

  (let ((data "\302\202B1")
	(ids (make-hash-table)))
    (maphash
     (lambda (index cell)
       (let ((id (car cell))
	     (amount (cadr cell))
	     (price 1))
	 (unless (gethash id ids)
	   (puthash id t ids)
	   (setq data (concat
		       data
		       (tmwchat-encode-base94 id 2)
		       (tmwchat-encode-base94 price 4)
		       (tmwchat-encode-base94 amount 3))))))
     tmwchat-player-inventory)

    (concat data (string (r) (r)))))

(defun tmwchat-parse-shopcmd (msg)
  (let ((result) (id 0) (price 0) (amount 0)
	(w (split-string msg)))
    (when (= (length w) 4)
      (setq id (string-to-int (nth 1 w))
	    price (string-to-int (nth 2 w))
	    amount (string-to-int (nth 3 w)))
      (when (and (> id 0)
		 (> price 0)
		 (> amount 0))
	(setq tmwchat--trade-item-id id
	      tmwchat--trade-item-price price
	      tmwchat--trade-item-amount amount
	      result t)))
    result))

(defun tmwchat-sell-to (nick &optional item-id price amount)

  (defun is-item-selling (id)
    (let (result)
      (dolist (s-info tmwchat-trade-selling)
	(when (= id (car s-info))
	  (setq result s-info)))
      result))

  (let* ((player-id (tmwchat-find-player-id nick))
	 (item-id (or item-id tmwchat--trade-item-id))
	 (price (or price tmwchat--trade-item-price))
	 (amount (or amount tmwchat--trade-item-amount))
	 (selling (or (and (member nick tmwchat-shop-admins)
			   (list item-id price))
		      (is-item-selling item-id))))
    (unless (member nick tmwchat-blocked-players)
      (if (member player-id tmwchat-nearby-player-ids)
	  (if selling
	      (if (>= (tmwchat-inventory-item-amount item-id)
		      amount)
		  (let ((real-price (cadr selling)))
		    (setq tmwchat--trade-player nick
			  tmwchat--trade-mode 'sell
		          tmwchat--trade-player-should-pay
			  (* amount real-price))
		    (tmwchat-trade-request player-id))
		(whisper-message nick "I don't have enough." t))
	    (whisper-message nick "I don't sell that." t))
	(whisper-message nick "I don't see you nearby." t)))))


(defun tmwchat-buy-from (nick &optional item-id price amount)

  (defun is-item-buying (id)
    (let (result)
      (dolist (s-info tmwchat-trade-buying)
	(when (= id (car s-info))
	  (let* ((max-amount (caddr s-info))
		 (inv-amount (tmwchat-inventory-item-amount id)))
	    (when (< inv-amount max-amount)
	      (setq result (cons (cadr s-info)
				 (- max-amount inv-amount)))))))
      result))

  (let* ((player-id (tmwchat-find-player-id nick))
	 (item-id (or item-id tmwchat--trade-item-id))
	 (price (or price tmwchat--trade-item-price))
	 (amount (or amount tmwchat--trade-item-amount))
	 (buying (is-item-buying item-id)))
    (unless (member nick tmwchat-blocked-players)
      (if (member player-id tmwchat-nearby-player-ids)
	  (if buying
	      (if (<= amount (cdr buying))
		  (let* ((real-price (* amount (car buying))))
		    (if (>= tmwchat-money real-price)
			(progn
			  (setq tmwchat--trade-player nick
				tmwchat--trade-shop-should-pay real-price
				tmwchat--trade-mode 'buy)
			  (tmwchat-trade-request player-id))
		      (whisper-message nick "I can't afford that." t)))
		(whisper-message nick "I don't need that much." t))
	    (whisper-message nick "I don't buy that." t))
	(whisper-message nick "I don't see you nearby." t)))))


(defun tmwchat-trade-give-zeny (nick &optional zeny)
  (let ((player-id (tmwchat-find-player-id nick)))
    (if (member player-id tmwchat-nearby-player-ids)
	(let ((zeny (or zeny tmwchat-money)))
	  (setq tmwchat--trade-mode 'money
		tmwchat--trade-player nick
		tmwchat--trade-shop-should-pay zeny)
	  (tmwchat-trade-request player-id))
      (whisper-message nick "I don't see you nearby." t))))


(defun trade-request (info)
  (let ((nick (bindat-get-field info 'nick))
	(spec   '((opcode       u16r)
		  (code         u8))))
    (tmwchat-trade-log "Trade request from %s" nick)
    (when tmwchat-shop-mode
      (whisper-message nick (tmwchat-selllist) t))
    (tmwchat-send-packet spec
			 (list (cons 'opcode #xe6)
			       (cons 'code 4)))))  ;; reject

(defun list-random-element (lst)
  (let ((len (length lst)))
    (nth (random len) lst)))

(defun trade-response (info)
  (let ((code (bindat-get-field info 'code)))
    (cond
     ((= code 0)
      (tmwchat-trade-log "Trade response: %s is too far away" tmwchat--trade-player)
      (whisper-message tmwchat--trade-player "You are too far away." t)
      (tmwchat--trade-reset-state))

     ((= code 3)
      (tmwchat-trade-log "Trade from %s accepted." tmwchat--trade-player)
      (run-with-timer 120 nil 'tmwchat-trade-cancel-request)
;      (setq tmwchat--trade-cancel-timer
;	    (run-at-time 120 nil 'tmwchat-trade-cancel-request))
      (cond
       ((eq tmwchat--trade-mode 'sell)
	;; (whisper-message
	;;  tmwchat--trade-player
	;;  (format "That will cost %d GP." tmwchat--trade-player-should-pay)
	;;  t)
	(let ((index (tmwchat-inventory-item-index tmwchat--trade-item-id)))
	  (if (> index -10)
	      (let ((tmwchat-delay-between-messages 0.1)
		    (gift-id))
		(tmwchat-trade-add-item index tmwchat--trade-item-amount)
		(when (setq gift-id (list-random-element tmwchat-shop-gift-ids))
		  (let ((gift-index (tmwchat-inventory-item-index gift-id)))
		    (when (> gift-index 0)
		      (tmwchat-trade-add-item gift-index 1))))
		(tmwchat-trade-add-complete))
	    (progn
	      (tmwchat-trade-cancel-request)
	      (tmwchat-trade-log "I cancel trade.")))))

       ((eq tmwchat--trade-mode 'buy)
	;; (whisper-message
	;;  tmwchat--trade-player
	;;  (format "I offer %d GP." tmwchat--trade-shop-should-pay)
	;;  t)
	(tmwchat-trade-add-item 0 tmwchat--trade-shop-should-pay)
	(puthash 0 tmwchat--trade-shop-should-pay tmwchat--trade-give-ids)
	(tmwchat-trade-log "I add %d GP." tmwchat--trade-shop-should-pay)
	(tmwchat-trade-add-complete))
       ((eq tmwchat--trade-mode 'money)
	;; (whisper-message
	;;  tmwchat--trade-player
	;;  (format "Transferring %d GP." tmwchat--trade-shop-should-pay)
	;;  t)
	(tmwchat-trade-add-item 0 tmwchat--trade-shop-should-pay)
	(puthash 0 tmwchat--trade-shop-should-pay tmwchat--trade-give-ids)
	(tmwchat-trade-log "I add %d GP." tmwchat--trade-shop-should-pay)
	(tmwchat-trade-add-complete))
       (t
	(tmwchat-trade-log "Trade error. Trade mode is %s" tmwchat--trade-mode)
	(tmwchat-trade-cancel-request))))

     ((= code 4)
      (tmwchat--trade-reset-state)
      (tmwchat-trade-log "Trade canceled." tmwchat--trade-player)))))

(defun trade-item-add (info)
  (let ((amount (bindat-get-field info 'amount))
	(id (bindat-get-field info 'id)))
    (tmwchat-trade-log "%s added to trade %d %s"
		       tmwchat--trade-player
		       amount
		       (tmwchat-item-name id))
    (cond
     ((= id 0)
      (setq tmwchat--trade-player-offer amount)
      (puthash id amount tmwchat--trade-receive-ids))
     ((> id 0)
      (incf (gethash id tmwchat--trade-receive-ids 0) amount)
      (cond
       ((eq tmwchat--trade-mode 'sell)
	(whisper-message tmwchat--trade-player "I accept only GP." t)
	(tmwchat-trade-cancel-request))
       ((eq tmwchat--trade-mode 'buy)
	(setq tmwchat--trade-player-item-id id
	      tmwchat--trade-player-item-amount amount)
	(unless (and (= tmwchat--trade-item-id id)
		     (= tmwchat--trade-item-amount amount))
	  (whisper-message
	   tmwchat--trade-player
	   (format "You should give me %d [%s]."
		   tmwchat--trade-item-amount
		   (tmwchat-item-name tmwchat--trade-item-id t))
	   t)
	  (tmwchat-trade-cancel-request)))
       ((eq tmwchat--trade-mode 'money) t)
       (t
	(tmwchat-trade-log "Trade error. Trade mode is %s" tmwchat--trade-mode)
	(tmwchat-trade-cancel-request))))
     (t
      (tmwchat-trade-log "Trade error.")
      (whisper-message tmwchat--trade-player "Trade error.")
      (tmwchat-trade-cancel-request)))))

(defun trade-item-add-response (info)
  (let ((index (bindat-get-field info 'index))
	(amount (bindat-get-field info 'amount))
	(code (bindat-get-field info 'code)))
    (cond
     ((= code 0)
      (when (> amount 0)
	(let ((id (car (gethash index tmwchat-player-inventory '(0)))))
	  (incf (gethash id tmwchat--trade-give-ids 0) amount)
	  (tmwchat-trade-log "I added %d %s."
			     amount
			     (tmwchat-item-name id)))
	(player-inventory-remove (list (cons 'index index)
				       (cons 'amount amount)))))
     ((= code 1)
      (tmwchat-trade-log "%s is overweight" tmwchat--trade-player)
      (whisper-message tmwchat--trade-player "You seem to be overweight." t)
      (tmwchat-trade-cancel-request))
     ((= code 2)
      (tmwchat-trade-log "%s has no free slots" tmwchat--trade-player)
      (whisper-message tmwchat--trade-player "You don't have free slots." t)
      (tmwchat-trade-cancel-request))
     (t
      (tmwchat-trade-log "Unknown trade error.")
      (whisper-message tmwchat--trade-player "Unknown trade error." t)
      (tmwchat-trade-cancel-request)))))

(defun trade-cancel (info)
  (tmwchat-trade-log "Trade with %s canceled." tmwchat--trade-player)
  (cancel-function-timers 'tmwchat-trade-cancel-request)
;  (when (timerp tmwchat--trade-cancel-timer)
;    (cancel-timer tmwchat--trade-cancel-timer))
  (tmwchat--trade-reset-state))

(defun trade-ok (info)
  (let ((who (bindat-get-field info 'who)))
    (unless (= who 0)
      (tmwchat-trade-log "Trade OK: %s" tmwchat--trade-player)
      (cond
       ((eq tmwchat--trade-mode 'sell)
	(if (>= tmwchat--trade-player-offer tmwchat--trade-player-should-pay)
	    (tmwchat-trade-ok)
	  (progn
	    (whisper-message tmwchat--trade-player "Your offer makes me sad." t)
	    (tmwchat-trade-cancel-request))))
       ((eq tmwchat--trade-mode 'buy)
	(if (and (= tmwchat--trade-player-item-id tmwchat--trade-item-id)
		 (= tmwchat--trade-player-item-amount tmwchat--trade-item-amount))
	    (tmwchat-trade-ok)
	  (progn
	    (whisper-message
	     tmwchat--trade-player
	     (format "You should give me %d %s."
		     tmwchat--trade-item-amount
		     (tmwchat-item-name tmwchat--trade-item-id t))
	     t)
	    (tmwchat-trade-cancel-request))))
       ((eq tmwchat--trade-mode 'money)
	(tmwchat-trade-ok))
       (t
	(tmwchat-trade-log "Trade error. Trade mode is %s" tmwchat--trade-mode)
	(tmwchat-trade-cancel-request))))))


(defun trade-complete (info)
  (cond
   ((eq tmwchat--trade-mode 'sell)
    (tmwchat-trade-log "Trade with %s completed. I sold %d %s and got %d GP."
		       tmwchat--trade-player
		       tmwchat--trade-item-amount
		       (tmwchat-item-name tmwchat--trade-item-id)
		       tmwchat--trade-player-offer))
   ((eq tmwchat--trade-mode 'buy)
    (tmwchat-trade-log "Trade with %s completed. I bought %d %s for %d GP."
		       tmwchat--trade-player
		       tmwchat--trade-item-amount
		       (tmwchat-item-name tmwchat--trade-item-id)
		       tmwchat--trade-shop-should-pay))
   ((eq tmwchat--trade-mode 'money)
    (tmwchat-trade-log "Trade with %s completed. I transferred %d GP."
		       tmwchat--trade-player
		       tmwchat--trade-shop-should-pay)))
  (setq tmwchat-money (+ tmwchat-money tmwchat--trade-player-offer))
  (setq tmwchat-money (- tmwchat-money tmwchat--trade-shop-should-pay))
  (cancel-function-timers 'tmwchat-trade-cancel-request)
;  (when (timerp tmwchat--trade-cancel-timer)
;    (cancel-timer tmwchat--trade-cancel-timer))
  (run-hook-with-args 'tmwchat-after-trade-hook
		      tmwchat--trade-player
		      tmwchat--trade-give-ids
		      tmwchat--trade-receive-ids)
  (tmwchat--trade-reset-state))

(defun tmwchat-trade-add-item (index amount)
  (let ((spec  '((opcode       u16r)
		 (index        u16r)
		 (amount       u32r))))
    (tmwchat-send-packet spec
			 (list (cons 'opcode #x0e8)
			       (cons 'index index)
			       (cons 'amount amount)))))
(make-variable-buffer-local 'tmwchat-trade-add-item)

(defun tmwchat-trade-add-complete ()
  (write-u16 #x0eb))
(make-variable-buffer-local 'tmwchat-trade-add-complete)

(defun tmwchat-trade-ok ()
  (write-u16 #x0ef))
(make-variable-buffer-local 'tmwchat-trade-ok)

(defun tmwchat-trade-request (id)
  (let ((spec '((opcode     u16r)
	        (id     vec    4))))
    (tmwchat-send-packet spec
			 (list  (cons 'opcode #x0e4)
				(cons 'id     id)))))
(make-variable-buffer-local 'tmwchat-trade-request)

(defun tmwchat-trade-cancel-request ()
  (write-u16 #x0ed))
(make-variable-buffer-local 'tmwchat-trade-cancel-request)

(defun tmwchat--trade-reset-state ()
  (clrhash tmwchat--trade-give-ids)
  (clrhash tmwchat--trade-receive-ids)
  (setq tmwchat--trade-player ""
        tmwchat--trade-player-offer 0
        tmwchat--trade-player-should-pay 0
	tmwchat--trade-player-item-id 0
	tmwchat--trade-player-item-amount 0
	tmwchat--trade-item-id 0
	tmwchat--trade-item-amount 0
	tmwchat--trade-item-price 0
	tmwchat--trade-mode nil
	tmwchat--trade-shop-should-pay 0))

(defun tmwchat-trade-log (&rest args)
  (with-current-buffer (get-buffer-create "TMWChat-trade")
    (let ((msg (apply 'format args)))
      (setq msg (format "[%s] %s" (tmwchat-time) msg))
      (goto-char (point-max))
      (insert msg)
      (newline))))

(defun tmwchat--item-names (table)
  (let (item-repr)
    (maphash
     (lambda (id amount)
       (let ((name (tmwchat-item-name id t)))
	 (push
	  (if (> amount 1)
	      (format "%d %s" amount name)
	    name)
	  item-repr)))
     table)
    (mapconcat 'identity item-repr ", ")))


(defun tmwchat-show-successful-trade (with gave received)
  "Echo info about successful trade in log."
  (defun str-or (s1 s2)
    (if (> (length s1) 0)
	s1
      s2))
  (let ((gave-repr (str-or (tmwchat--item-names gave) "nothing"))
	(received-repr (str-or (tmwchat--item-names received) "nothing")))
    (tmwchat-log "[TRADE:%s] give %s, get %s, zeny %d"
		 with gave-repr received-repr tmwchat-money)))

(defun tmwchat-trade-notify-admins (with gave received)
  "Whisper to online admins about successful trade."
  (defun str-or (s1 s2)
    (if (> (length s1) 0)
	s1
      s2))
  (let ((gave-repr (str-or (tmwchat--item-names gave) "nothing"))
	(received-repr (str-or (tmwchat--item-names received) "nothing"))
	(onl (tmwchat-get-online-users))
	(tmwchat-delay-between-messages 4))
    (dolist (p tmwchat-shop-admins)
      (when (member p onl)
	(whisper-message
	 p
	 (format "[TRADE:%s] give %s, get %s, zeny %d"
		 with gave-repr received-repr tmwchat-money)
	 t)))))

(provide 'tmwchat-trade)
