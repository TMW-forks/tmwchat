(require 'tmwchat-log)

(defun tmwchat-decode-string (s)
  (condition-case nil
      (decode-coding-string s 'utf-8 t)
    (error "{{STRING_DECODE_ERROR}}")))

(defun tmwchat--contains-302-202 (str)
  "Check if string contains ManaPlus-specific messages with \302\202
   that breaks utf8 decoding"
  (string-match-p "#o302|#o202" str))

(defun vec-less (v1 v2)
  (or (< (elt v1 3) (elt v2 3))
      (and (= (elt v1 3) (elt v2 3))
	   (or (< (elt v1 2) (elt v2 2))
	       (and (= (elt v1 2) (elt v2 2))
		    (or (< (elt v1 1) (elt v2 1))
			(and (= (elt v1 1) (elt v2 1))
			     (< (elt v1 0) (elt v2 0)))))))))

(make-variable-buffer-local 'vec-less)

(defun tmwchat-read-coordinates (v)
  (let ((x) (y)
	(v0 (elt v 0))
	(v1 (elt v 1))
	(v2 (elt v 2)))
    (message "tmwchat-read-coordinates %s" v)
    (setq x (lsh (logior (lsh v0 8)
			 (logand v1 #xc0))
		 -6))
    (setq y (lsh (logior (lsh (logand v1 #x3f) 8)
			 (logand v2 #xf0))
		 -4))
    (cons x y)))
(make-variable-buffer-local 'tmwchat-read-coordinates)

(defun tmwchat-read-coordinate-pair (v)
  (let ((x1) (y1) (x2) (y2)
	(v0 (elt v 0))
	(v1 (elt v 1))
	(v2 (elt v 2))
	(v3 (elt v 3))
	(v4 (elt v 4)))
    (setq x1 (lsh (logior (lsh v0 8)
			  (logand v1 #xc0))
		  -6))
    (setq y1 (lsh (logior (lsh (logand v1 #x3f) 8)
			  (logand v2 #xf0))
		  -4))
    (setq x2 (lsh (logior (lsh (logand v2 #x0f) 8)
			  v3)
		  -2))
    (setq y2 (logior (lsh (logand v3 #x03) 8) v4))
    (list x1 y1 x2 y2)))
(make-variable-buffer-local 'tmwchat-read-coordinate-pair)

(defun tmwchat-coordinate-pair-to-vector (x1 y1 x2 y2)
  (let
      ((v0 (lsh x1 -2))
       (v1 (logior
	    (lsh (logand x1 #x03) 6)
	    (lsh (logand y1 #x3f0) -4)))
       (v2 (logior
	    (lsh (logand y1 #x0f) 4)
	    (lsh (logand x2 #x3c0) -6)))
       (v3 (logior
	    (lsh (logand x2 #x3f) 2)
	    (lsh (logand y2 #x300) -8)))
       (v4 (logand y2 #xff)))
    (vector v0 v1 v2 v3 v4)))

(defun tmwchat-coordinate-to-vector (x y)
  (let
      ((v0 (lsh x -2))
       (v1 (logior
	    (lsh (logand x #x03) 6)
	    (lsh (logand y #x3f0) -4)))
       (v2 (lsh (logand y #x0f) 4)))
    (vector v0 v1 v2)))

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

(defun tmwchat-make-read-only ()
  "Make all the text in the current buffer read-only."
  (add-text-properties (point-min) tmwchat--start-point
		       '(read-only t front-sticky t rear-nonsticky t)))

(defun tmwchat-find-player-id (name)
  "Find player ID by name."
  (let ((found-id -1))
    (maphash
     (lambda (id pname)
       (when (string-equal name pname)
	 (setq found-id id)))
     tmwchat-player-names)
    found-id))

(defun tmwchat-read-itemdb (dbfile &optional dbhash)
  (interactive "fPath to itemdb.txt: ")
  (with-temp-buffer
    (message "Loading itemdb from %s" dbfile)
    (insert-file-contents dbfile)
    (let ((itemsdb (or dbhash (make-hash-table :test 'equal))))
      (while (not (eobp))
	(let ((line (buffer-substring-no-properties
		     (line-beginning-position)
		     (line-end-position)))
	      (item-id) (item-name))
	  (when (string-match "^\\([0-9]+\\)[\t ]+\\(.+\\)" line)
	    (setq item-id
		  (string-to-int
		   (substring line (match-beginning 1) (match-end 1)))
		  item-name
		  (substring line (match-beginning 2) (match-end 2)))
	    (puthash item-id item-name itemsdb)))
	(forward-line 1))
      itemsdb)))

(defun tmwchat-item-name (id &optional link)
  "Get item [name] from itemdb.
When link is non-nil, return [@@id|name@@]"
  (if (= id 0) "GP"
    (let ((name (gethash id tmwchat-itemdb "Unknown")))
      (if link
	  (format "[@@%d|%s@@]" id name)
	(format "[%s]" name)))))

(provide 'tmwchat-util)
