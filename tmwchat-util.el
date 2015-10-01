(require 'tmwchat-log)

(defun tmwchat-decode-string (s)
  (condition-case nil
      (decode-coding-string s 'utf-8 t)
    (error "{{STRING_DECODE_ERROR}}")))

(defun tmwchat--contains-302-202 (str)
  "Check if string contains ManaPlus-specific messages with \302\202
   that breaks utf8 decoding"
  (string-match-p "^#o302|#o202" str))

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
  (let ((x) (y))
    (setq x (lsh (logior (lsh (logand (elt v 1) #xc0)
			      8)
			 (elt v 0))
		 -6))
    (setq y (lsh (logior (lsh (logand (elt v 2) #xf0)
			      8)
			 (logand (elt v 1) #x3f))
		 -4))
    (cons x y)))
(make-variable-buffer-local 'tmwchat-read-coordinates)

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

(provide 'tmwchat-util)
