
(defvar tmwchat-player-inventory (make-hash-table :test 'equal)
  "Hash table containing player inventory and equipment")

(defconst tmwchat-inventory-offset 2)


(defun player-inventory (info)
  (let ((items (bindat-get-field info 'items)))
    (dolist (item items)
      (let ((amount (cdr (assoc 'amount item)))
	    (id (cdr (assoc 'id item)))
	    (index (cdr (assoc 'index item))))
	(puthash index (list id amount) tmwchat-player-inventory)))))


(defun player-equipment (info)
  (let ((items (bindat-get-field info 'items)))
    (dolist (item items)
      (let ((amount 1)
	    (id (cdr (assoc 'id item)))
	    (index (cdr (assoc 'index item))))
	(puthash index (list id amount) tmwchat-player-inventory)))))


(defun player-inventory-add (info)
  (let ((id (bindat-get-field info 'id))
	(amount (bindat-get-field info 'amount))
	(index (bindat-get-field info 'index))
	(inv-cell))
    (if (setq inv-cell (gethash index tmwchat-player-inventory))
	(let ((curr-amount (cadr inv-cell)))
	  (setq inv-cell (list id (+ curr-amount amount))))
      (setq inv-cell (list id amount)))
    (puthash index inv-cell tmwchat-player-inventory)))


(defun player-inventory-remove (info)
  (let ((amount (bindat-get-field info 'amount))
	(index (bindat-get-field info 'index))
	(inv-cell))
    (if (setq inv-cell (gethash index tmwchat-player-inventory))
	(let ((cell-amount (cadr inv-cell))
	      (cell-id (car inv-cell)))
	  (if (>= amount cell-amount)
	      (remhash index tmwchat-player-inventory)
	    (puthash index (list cell-id (- cell-amount amount))
		     tmwchat-player-inventory)))
      (tmwchat-log "player-inventory-remove error: %s" info))))


(defun tmwchat-inventory-item-index (item-id)
  (let ((index -10))
    (maphash
     (lambda (cell-index cell)
       (when (= (car cell) item-id)
	 (setq index cell-index)))
     tmwchat-player-inventory)
    index))


(defun tmwchat-inventory-item-amount (item-id)
  (let ((amount 0))
    (maphash
     (lambda (cell-index cell)
       (when (= (car cell) item-id)
	 (setq amount (cadr cell))))
     tmwchat-player-inventory)
    amount))

(provide 'tmwchat-inventory)
