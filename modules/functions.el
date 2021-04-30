
;;;; Misc. functions
(defun s/kill-buffer-and-frame ()
  "Kill the current buffer and delete the selected frame."
  (interactive)
  (let ((frame-to-delete (selected-frame))
	(buffer-to-kill (current-buffer))
	(kill-buffer-query-functions nil)
	(delete-frame-functions (lambda () (ignore-errors (delete-frame)))))
    (unwind-protect
	(progn
	  (add-hook 'kill-buffer-hook delete-frame-functions t t)
	  (if (kill-buffer (current-buffer))
	      ;; If `delete-frame' failed before, we rerun it to regenerate
	      ;; the error so it can be seen in the echo area.
	      (when (eq (selected-frame) frame-to-delete)
		(delete-frame))))
      ;; If the buffer is not dead for some reason (probably because
      ;; of a `quit' signal), remove the hook again.
      (ignore-errors
	(with-current-buffer buffer-to-kill
	  (remove-hook 'kill-buffer-hook delete-frame-functions t))))))


(defun s/choose-from-list (prompt var-list &optional var-name)
  (if (not var-name)
      (setq var-name
	    (ivy-read prompt var-list)))
  (nth 1 (assoc var-name var-list)))

(defun s/kill-this-buffer ()
  "Kill the current buffer without any prompts"
  (interactive)
  (kill-buffer (current-buffer)))

(defun s/go-to-config ()
  "Go to the emacs config-file"
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

