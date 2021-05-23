

(use-package vterm
  :init

  (defun s/persp-generate-process-name (process make-new)
    "Infer the buffer name for PROCESS or generate a new one if MAKE-NEW is true.
This function is heavily inspired by the function projectile-generate-process-name"
    (let* ((persp (persp-current-name))
           (base-name (format "*%s %s*" process persp)))
      (if make-new
          (generate-new-buffer-name base-name)
	base-name)))

  (defun s/persp-toggle-vterm (&optional arg)
    "Invoke `vterm' in the active perspective.
Switch to the project specific term buffer if it already exists.
Use a prefix argument ARG to indicate creation of a new process instead.
This function is heavily inspired by the functions projectile-run-vterm"
    (interactive "P")
    (let* ((buffer (s/persp-generate-process-name "vterm" arg))
	   (active-buffer (buffer-name)))
      (unless (buffer-live-p (get-buffer buffer))
        (vterm buffer))
      (if (string= buffer active-buffer)
	  (delete-window)
	(progn
	  (display-buffer buffer)
	  (switch-to-buffer-other-window buffer)))))

  :config
  ;; Place vterm-buffers in a separate window at the bottom of the screen
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3)))

  :general
  (s/leader-def
    "o t" '(s/persp-toggle-vterm :wk "terminal"))

  )

(use-package vterm-toggle
  :general
  (s/local-leader-def
    :keymaps '(vterm-mode-map vterm-copy-mode-map)
    "c" '(vterm-toggle :wk "cd to last buffer"))
)

