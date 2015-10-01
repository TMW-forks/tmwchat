(require 'tmwchat-log)

(defun tmwchat-decode-string (s)
  ;; (condition-case nil
      (decode-coding-string s 'utf-8 t))
    ;; (error "{{STRING_DECODE_ERROR}}")))

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

(provide 'tmwchat-util)
