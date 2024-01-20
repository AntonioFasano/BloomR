;;; br-simple-buffer-menu.el --- BRemacs init library -*- lexical-binding: t -*-

;; Copyright (C) Antonio Fasano
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GPL 2+ license.

;;; Commentary:
;; This package redefine the Buffers menu to show only the interesting
;; buffers defined with variable `br-regular-buffer-regex'.

;;; Code:

(defvar br-regular-buffer-regex "^\\*R\\|[^]*]$"  "Regexp identifing interesting buffers.
Currently those whose name starts with '*R' or does not end with ']', '*'.")

(defun br-next ()
  "Switch to next buffer among those returned `br-file-buffers-regular`"
  (interactive)
  (let ((buffers (br-file-buffers-regular)))
      (if (< (length buffers) 2)
           (switch-to-buffer (nth 0 buffers))
          (bury-buffer-internal (nth 0 buffers))
          (switch-to-buffer (nth 1 buffers)))))


(defun br-previous()
  "Switch to previous buffer among those returned `br-file-buffers-regular`"
  (interactive)
  (let* ((buffers (br-file-buffers-regular))
	 (prev (car (last buffers))))
      (if (< (length buffers) 2)
           (switch-to-buffer (nth 0 buffers))
          (dolist (elt (butlast buffers))
              (bury-buffer-internal elt))
          (switch-to-buffer prev))))


(defun br-file-buffers-regular ()
  "Filter (buffer-list) for buffers whose name matches `br-regular-buffer-regex'."
  (delq nil (mapcar #'(lambda (buf)
			(and ; (buffer-file-name buf) restrict to file buffer only
			 (let ((case-fold-search nil))
			   (string-match "^\\*R\\|[^]*]$" (buffer-name buf)))
			     buf))
		    (buffer-list))))

(defun menu-bar-update-buffers (&optional force)
  "Hacked version of the same function in `buffer-menu.el',
filtering buffers with `br-file-buffers-regular',"
  
  ;; If user discards the Buffers item, play along.
  (and (lookup-key (current-global-map) [menu-bar buffer])
       (or force (frame-or-buffer-changed-p))
       (let ((buffers (br-file-buffers-regular))
	     (frames (frame-list))
	     buffers-menu)

	 ;; Make the menu of buffers proper.
	 (setq buffers-menu
               (let ((i 0)
                     (limit (if (and (integerp buffers-menu-max-size)
                                     (> buffers-menu-max-size 1))
                                buffers-menu-max-size most-positive-fixnum))
                     alist)
		 ;; Put into each element of buffer-list
		 ;; the name for actual display,
		 ;; perhaps truncated in the middle.
                 (while buffers
                   (let* ((buf (pop buffers))
                          (name (buffer-name buf)))
                     (unless (eq ?\s (aref name 0))
                       (push (menu-bar-update-buffers-1
                              (cons buf
				    (if (and (integerp buffers-menu-buffer-name-length)
					     (> (length name) buffers-menu-buffer-name-length))
					(concat
					 (substring
					  name 0 (/ buffers-menu-buffer-name-length 2))
					 "..."
					 (substring
					  name (- (/ buffers-menu-buffer-name-length 2))))
				      name)
                                    ))
                             alist)
                       ;; If requested, list only the N most recently
                       ;; selected buffers.
                       (when (= limit (setq i (1+ i)))
                         (setq buffers nil)))))
		 (list (menu-bar-buffer-vector alist))))

	 ;; Make a Frames menu if we have more than one frame.
	 (when (cdr frames)
	   (let* ((frames-vec (make-vector (length frames) nil))
                  (frames-menu
                   (cons 'keymap
                         (list "Select Frame" frames-vec)))
                  (i 0))
             (dolist (frame frames)
               (aset frames-vec i
                     (cons
                      (frame-parameter frame 'name)
                      `(lambda ()
                         (interactive) (menu-bar-select-frame ,frame))))
               (setq i (1+ i)))
	     ;; Put it after the normal buffers
	     (setq buffers-menu
		   (nconc buffers-menu
			  `((frames-separator "--")
			    (frames menu-item "Frames" ,frames-menu))))))

	 ;; Add in some normal commands at the end of the menu.  We use
	 ;; the copy cached in `menu-bar-buffers-menu-command-entries'
	 ;; if it's been set already.  Note that we can't use constant
	 ;; lists for the menu-entries, because the low-level menu-code
	 ;; modifies them.
	 (unless menu-bar-buffers-menu-command-entries
	   (setq menu-bar-buffers-menu-command-entries
		 (list '(command-separator "--")
		       (list 'next-buffer
			     'menu-item
			     "Next Buffer"
			     'br-next
			     :help "Switch to the \"next\" buffer in a cyclic order")
		       (list 'previous-buffer
			     'menu-item
			     "Previous Buffer"
			     'br-previous
			     :help "Switch to the \"previous\" buffer in a cyclic order")
		       (list 'select-named-buffer
			     'menu-item
			     "Select Named Buffer..."
			     'switch-to-buffer
			     :help "Prompt for a buffer name, and select that buffer in the current window")
		       (list 'list-all-buffers
			     'menu-item
			     "List All Buffers"
			     'list-buffers
			     :help "Pop up a window listing all Emacs buffers"
			     ))))
	 (setq buffers-menu
	       (nconc buffers-menu menu-bar-buffers-menu-command-entries))

         ;; We used to "(define-key (current-global-map) [menu-bar buffer]"
         ;; but that did not do the right thing when the [menu-bar buffer]
         ;; entry above had been moved (e.g. to a parent keymap).
	 (setcdr global-buffers-menu-map (cons "Buffers" buffers-menu)))))


(defun br-init-simple-menu ()
  (setq menu-bar-buffers-menu-command-entries nil)
  (menu-bar-update-buffers t))

(provide 'br-simple-buffer-menu)

;;; br-simple-buffer-menu.el ends here

