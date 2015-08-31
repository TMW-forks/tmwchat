(require 'cl)
(require 'speedbar)
(require 'easymenu)

(defvar tmwchat-recent-users (make-ring 4))

(defun tmwchat--update-recent-users (nick)
  (unless (member nick (ring-elements tmwchat-recent-users))
    (setq tmwchat--speedbar-dirty t)
    (ring-insert tmwchat-recent-users nick)))

(defvar tmwchat-speedbar-key-map
  (let ((map (speedbar-make-specialized-keymap)))
    (define-key map "t" 'tmwchat--speedbar-test)
    map))

(setq tmwchat--speedbar-dirty t)

(defun tmwchat-speedbar-item-info ()
  (or (speedbar-item-info-file-helper)
      "tmwchat-speedbar-item-info (tag)"))

(defun tmwchat--speedbar-test ()
  (message "Speedbar test function"))

(defvar tmwchat-speedbar-menu-items
  '("TMWChat"
    ["Test" tmwchat--speedbar-test t]))

(defun tmwchat--speedbar-item-clicked (nick id arg3)
  (select-frame-set-input-focus tmwchat--frame)
  (select-window tmwchat--window)
  (tmwchat--replace-whisper-cmd nick))

(defun tmwchat-speedbar-buttons (directory &optional depth)
  (when tmwchat--speedbar-dirty
    (let ((nearby tmwchat--beings)
	  (online (tmwchat-get-online-users))
	  (recent (ring-elements tmwchat-recent-users)))
      (erase-buffer)
      (speedbar-insert-separator "Nearby")
      (maphash (lambda (id nick)
		 (push nick nearby)
		 (speedbar-insert-button
		  nick
		  'speedbar-button-face
		  'speedbar-highlight-face
		  'tmwchat--speedbar-item-clicked
		  id))
	       nearby)
      (speedbar-insert-separator "Recent")
      (mapc (lambda (nick)
		 (speedbar-insert-button
		  nick
		  'speedbar-button-face
		  'speedbar-highlight-face
		  'tmwchat--speedbar-item-clicked
		  nil))
	    (sort recent 'string-lessp))
      ;; (speedbar-insert-separator "Social")
      ;; (speedbar-insert-button "Party"
      ;; 			      'speedbar-button-face
      ;; 			      'speedbar-highlight-face
      ;; 			      'tmwchat--speedbar-item-clicked
      ;; 			      nil)
      ;; (speedbar-insert-button "Guild"
      ;; 			      'speedbar-button-face
      ;; 			      'speedbar-highlight-face
      ;; 			      'tmwchat--speedbar-item-clicked
      ;; 			      nil)
      (speedbar-insert-separator (format "Online (%s)" (length online)))
      (mapc (lambda (nick)
		 (speedbar-insert-button
		  nick
		  'speedbar-button-face
		  'speedbar-highlight-face
		  'tmwchat--speedbar-item-clicked
		  nil))
	    (sort online 'string-lessp))
      (setq tmwchat--speedbar-dirty nil))))

(defun tmwchat-install-speedbar-variables ()
  (speedbar-add-expansion-list '("TMWChat"
				 tmwchat-speedbar-menu-items
				 tmwchat-speedbar-key-map
				 tmwchat-speedbar-buttons))
  (speedbar-add-mode-functions-list
   '("TMWChat"
     (speedbar-item-info . tmwchat-speedbar-item-info)))
  )

(if (featurep 'speedbar)
    (tmwchat-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'tmwchat-install-speedbar-variables))

(provide 'tmwchat-speedbar)
