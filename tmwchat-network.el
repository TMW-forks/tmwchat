(require 'cl)
(require 'bindat)

(setq tmwchat--partial-packet nil)

(defun tmwchat-send-packet (spec data &optional process)
  (let ((process (or process tmwchat--client-process))
	(bin-data (bindat-pack spec data)))
    (process-send-string process bin-data)))

;;; unknown request skipping
(defconst tmwchat--packet-lenghts
  '(  10    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
       0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
       0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
       0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
       0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
       0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
       0   50    3   -1   55   17    3   37   46   -1   23   -1    3  108    3    2
       3   28   19   11    3   -1    9    5   54   53   58   60   41    2    6    6
       7    3    2    2    2    5   16   12   10    7   29   23   -1   -1   -1    0
       7   22   28    2    6   30   -1   -1    3   -1   -1    5    9   17   17    6
      23    6    6   -1   -1   -1   -1    8    7    6    7    4    7    0   -1    6
       8    8    3    3   -1    6    6   -1    7    6    2    5    6   44    5    3
       7    2    6    8    6    7   -1   -1   -1   -1    3    3    6    6    2   27
       3    4    4    2   -1   -1    3   -1    6   14    3   -1   28   29   -1   -1
      30   30   26    2    6   26    3    3    8   19    5    2    3    2    2    2
       3    2    6    8   21    8    8    2    2   26    3   -1    6   27   30   10
       2    6    6   30   79   31   10   10   -1   -1    4    6    6    2   11   -1
      10   39    4   10   31   35   10   18    2   13   15   20   68    2    3   16
       6   14   -1   -1   21    8    8    8    8    8    2    2    3    4    2   -1
       6   86    6   -1   -1    7   -1    6    3   16    4    4    4    6   24   26
      22   14    6   10   23   19    6   39    8    9    6   27   -1    2    6    6
     110    6   -1   -1   -1   -1   -1    6   -1   54   66   54   90   42    6   42
      -1   -1   -1   -1   -1   30   -1    3   14    3   30   10   43   14  186  182
      14   30   10    3   -1    6  106   -1    4    5    4   -1    6    7   -1   -1
       6    3   106   10   10  34    0    6    8    4    4    4   29   -1   10    6
      90   86   24    6   30  102    9    4    8    4   14   10    4    6    2    6
       3    3   35    5   11   26   -1    4    4    6   10   12    6   -1    4    4
      11    7   -1   67   12   18  114    6    3    6   26   26   26   26    2    3
       2   14   10   -1   22   22    4    2   13   97    0    9    9   29    6   28
       8   14   10   35    6    8    4   11   54   53   60    2   -1   47   33    6
      30    8   34   14    2    6   26    2   28   81    6   10   26    2   -1   -1
      -1   -1   20   10   32    9   34   14    2    6   48   56   -1    4    5   10
      26    0    0    0   18    0    0    0    0    0    0   19   10    0    0    0
       2   -1   16    0    8   -1    0    0    0    0    0    0    0    0    0    0
      -1  122   -1   -1   -1   -1   10   -1   -1    0    0    0    0    0    0    0
      ))


;;----------------------------------------------------------------------
(defconst tmwchat--u16-spec
  '((opcode        u16r)))

(defun write-u16 (arg)
  (tmwchat-send-packet tmwchat--u16-spec
		       (list (cons 'opcode arg))))
(make-variable-buffer-local 'write-u16)

(defun dispatch (packet packet-specs)
  
  (defun request-length (key)
    (cond ((= key #x7531) 10)
	  ((= key #x7534) -1)
	  ((and (>= key 0)
		(< key (length tmwchat--packet-lenghts)))
	   (nth key tmwchat--packet-lenghts))
	  (t -1)))
  
  (when tmwchat--partial-packet
    (setq packet (concat tmwchat--partial-packet packet)
	  tmwchat--partial-packet nil))

  (let ((opcode (bindat-get-field
		(bindat-unpack '((opcode  u16r)) packet)
		'opcode))
	(plength (length packet)))
    (cond
     ((assoc opcode packet-specs)
      (let ((expected-len)
	    (parsed-data)
	    (spec (nth 1 (assoc opcode packet-specs)))
	    (fun (nth 2 (assoc opcode packet-specs))))
	(condition-case nil
	    (progn
	      (if (numberp spec)
		  (setq expected-len spec)
		(setq parsed-data (bindat-unpack spec packet 2)
		      expected-len (bindat-length spec parsed-data)))
	      (unless (numberp spec)
		(when (functionp fun)
		  (funcall fun parsed-data)))
	      (when (> plength (+ expected-len 2))
		(let ((new-packet (substring packet (+ expected-len 2))))
		  (dispatch new-packet packet-specs))))
	  (args-out-of-range
	   (setq tmwchat--partial-packet (concat tmwchat--partial-packet packet))))))
     (t
      (let ((expected-len (request-length opcode)))
	(condition-case nil
	    (progn
	      (when (eq expected-len -1)
		(setq expected-len    ;; get the next u16r
		      (bindat-get-field
		       (bindat-unpack '((fill 2) (len u16r)) packet)
		       'len)))
	      (when (> plength expected-len)
		(let ((new-packet (substring packet expected-len)))
		 (dispatch new-packet packet-specs))))
	  (args-out-of-range
	   (setq tmwchat--partial-packet (concat tmwchat--partial-packet packet)))))))))

(provide 'tmwchat-network)
