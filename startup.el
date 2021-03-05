;;;; Startup optimisation

;; Set garbage collection threshold
;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(defvar s/gc-cons-threshold (* 1024 1024 50)) ; Threshold for garbage disposal

(setq gc-cons-threshold (* 1024 1024 100))

;; Set file-name-handler-alist
;; Also from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq file-name-handler-alist-original file-name-handler-alist
      file-name-handler-alist nil)

;; Reset all variables after startup is finished
(defun s/reset-vars ()
  (setq gc-cons-threshold s/gc-cons-threshold)
  (setq file-name-handler-alist file-name-handler-alist-original)
  (makunbound 'file-name-handler-alist-original)
  (message "gc-cons-threshold and file-name-handler-alist restored"))
(run-with-idle-timer 5 nil 's/reset-vars)

;;;; Bootstrap straight.el

;; https://github.com/raxod502/straight.el/issues/356
(setq straight-recipes-emacsmirror-use-mirror t) ; Use a mirror for emacsmirror

;; Perform the bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
;; Use straight together with use-package
(straight-use-package 'use-package) ; Install use-package
(setq straight-use-package-by-default t)

;;;; Import the shell environment
;; https://gitlab.com/vigou3/emacs-modified-macos/blob/master/default.el
;; Import some shell environment variables into Emacs at launch.
(use-package exec-path-from-shell
  :config
  ;; https://emacs.stackexchange.com/questions/29681/ess-r-startup-warning-locale
  (setq exec-path-from-shell-check-startup-files nil) ; Some variables should be set in .bashrc
  (nconc exec-path-from-shell-variables
	 '("LANG" "LC_ALL" "LANG" "R_PROFILE_USER"))
  (exec-path-from-shell-initialize))


;;;; BASIC SETTINGS

(defvar s/macos-p (string-equal system-type "darwin")) ; Is this a mac?

(setq inhibit-startup-screen t) ; Remove startup screen
(setq column-number-mode t) ; Display column numbers
(global-hl-line-mode) ; Highlight current line
(setq ring-bell-function 'ignore) ; Stop the error bell sound
(fset 'yes-or-no-p 'y-or-n-p) ; Change all prompts to y or n

(scroll-bar-mode -1) ; Remove scroll bar
(tool-bar-mode -1) ; Remove tool bar
(menu-bar-mode -1) ; Sometimes remove menu bar

;; ;; Autosave and backups
;; (setq make-backup-files nil) ; don't make backup files
;; (setq auto-save-default nil) ; Do not autosave

;; macOS stuff
(if s/macos-p
    (progn
     (setq mac-option-modifier nil ;; do not use the option key
	   mac-command-modifier 'meta) ;; command is meta
     (setq dired-use-ls-dired nil)))
 
;; Ignore case in completion
(setq completion-ignore-case t
      case-fold-search nil
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)
