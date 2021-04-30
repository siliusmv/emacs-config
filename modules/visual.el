
;;;; Change text size
;; This contains the functions default-text-scale-(increase/decrease)
(use-package default-text-scale)

;;;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; Resize frames
(use-package frame-cmds)

;;;; Themes
(use-package doom-themes
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
	doom-themes-enable-italic t)

  (defvar s/themes
    (list
     '("dark" doom-one)
     '("light" doom-one-light)))

  (defun s/choose-theme (&optional theme-short)
    (interactive)
    (let ((theme
	   (s/choose-from-list "Select theme: " s/themes theme-short)))
      (load-theme theme t)))

  (s/choose-theme s/init-theme)
  )


;;;; Modeline

(use-package all-the-icons :config (setq all-the-icons-scale-factor 1.0))

(use-package doom-modeline
  :config
  (setq
   ;; How tall the mode-line should be (only respected in GUI Emacs).
   doom-modeline-height 25
   ;; How wide the mode-line bar should be (only respected in GUI Emacs).
   doom-modeline-bar-width 3
   ;; If you are expereicing the laggy issue, especially while editing remote files
   ;; with tramp, please try `file-name' style.
   ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
   doom-modeline-buffer-file-name-style 'buffer-name
   ;; Whether show `all-the-icons' or not (if nil nothing will be showed).
   doom-modeline-icon t
   ;; Whether show the icon for major mode. It respects `doom-modeline-icon'.
   doom-modeline-major-mode-icon t
   ;; Display color icons for `major-mode'. It respects `all-the-icons-color-icons'.
   doom-modeline-major-mode-color-icon nil
   ;; If non-nil, only display one number for checker information if applicable.
   doom-modeline-checker-simple-format t
   ;; Whether display `lsp' state or not. Non-nil to display in mode-line.
   doom-modeline-lsp t
   ;; Whether display environment version or not
   doom-modeline-env-version t
   ;; Only show true name for symlinks
   find-file-visit-truename t
   ;; Display time
   display-time-format (format-time-string "%H:%M"))

  (doom-modeline-mode))
