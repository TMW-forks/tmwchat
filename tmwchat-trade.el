(require 'cl)

(defcustom tmwchat-trade-selling nil
  "List of selling items"
  :group 'tmwchat
  :type '(repeat (list :tag "Selling"
		       (integer :tag "ID")
		       (integer :tag "Price")
		       (integer :tag "Amount"))))

(defun tmwchat-encode-base94 (value size)
  (let ((output "")
	(base 94)
	(start 33))
    (while (> value 0)
      (setq output (concat output (list (+ (% value base) start)))
	    value (/ value base)))
    (while (< (length output) size)
      (setq output (concat output (list start))))
    output))

(defun tmwchat-selllist ()
  (let ((data "\302\202B1"))
    (dolist (item tmwchat-trade-selling)
      (let ((id (car item))
	    (price (caddr item))
	    (amount (cadr item)))
	(setq data (concat
		    data
		    (tmwchat-encode-base94 id 2)
		    (tmwchat-encode-base94 price 4)
		    (tmwchat-encode-base94 amount 3)))))
    data))

(provide 'tmwchat-trade)
