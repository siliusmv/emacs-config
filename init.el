;;; =========================================================
;;; %%%%%%%%%%%%%%%% My Emacs init file %%%%%%%%%%%%%%%%%%%%%
;;; =========================================================

;; TODO
;; Add (when (on-macos)) or similar to the init
;; Something is wrong with ESS ans line-numbers, I think!
;; Remove use of Ctrl from ESS
;; Get working functionality for workspaces
;; Create a which-key map for magit
;; Get full control of which-key


;; Global variables
(defvar mu4e-p nil)
(defvar init-theme "dark")
(defvar init-dict "british")
(defvar my-gc-cons-threshold (* 1024 1024 5))
  

;; =========================================================
;; Startup optimisation
;; From https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
;; =========================================================

;; Set garbage collection threshold
;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
; (setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

;; Set file-name-handler-alist
;; Also from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Set deferred timer to reset them
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold my-gc-cons-threshold)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))


;;; =========================================================
;;; Bootstrap straight.el
;;; =========================================================

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


;;;
;;; https://gitlab.com/vigou3/emacs-modified-macos/blob/master/default.el
;;; Import the shell environment
;;;
;; Import some shell environment variables into Emacs at launch. Steve
;; Purcell's exec-path-from-shell imports PATH and MANPATH by default;
;; LANG, TEXINPUTS and BIBINPUTS are added here. You can customize
;; 'exec-env-from-shell-variables' in site-start.el or the user's
;; config file.
(use-package exec-path-from-shell
  :config

  ;; https://emacs.stackexchange.com/questions/29681/ess-r-startup-warning-locale
  (exec-path-from-shell-copy-env "LC_ALL")
  (exec-path-from-shell-copy-env "LANG")

  (nconc exec-path-from-shell-variables '("LANG" "TEXINPUTS" "BIBINPUTS"))
  (exec-path-from-shell-initialize)
  )

;; macOS stuff
(setq mac-option-modifier nil ;; do not use the option key
      mac-command-modifier 'meta) ;; command is meta

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))




 
;; =========================================================
;; %%%%%%%%%%%%%%%%%%% BASIC SETTINGS %%%%%%%%%%%%%%%%%%%%%%
;; =========================================================
;; Remove menus and stuff
(setq inhibit-startup-screen t) ;; Remove startup screen

(setq column-number-mode t) ;; Display column numbers

(global-hl-line-mode) ;; Highlight current line


;; Allow enclosing a marked region with $$
(add-to-list 'insert-pair-alist (list ?\$ ?\$))

;; This must be done after loading GUI elements
(defun remove-all-bars (frame)
  (select-frame frame)
  (progn
    (toggle-scroll-bar -1)
    (menu-bar-mode -1)
    (tool-bar-mode -1)))

(add-hook 'after-make-frame-functions #'remove-all-bars)

(if (display-graphic-p)
  (progn
    (toggle-scroll-bar -1)
    (menu-bar-mode -1)
    (tool-bar-mode -1)))

;; Backup files
(setq make-backup-files nil) ;; Do not create backup files
;; Move all temporary backup files to /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Ignore case in completion
(setq completion-ignore-case t)
(setq case-fold-search nil)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; Add the themes-folder to load-path
(add-to-list 'custom-theme-load-path
	     (expand-file-name "~/.emacs.d/themes/"))

(xterm-mouse-mode t) ;; Enable mouse in terminal

(defun siliusmv/kill-buffer-and-frame ()
  "Kill the current buffer and delete the selected frame."
  (interactive)
  (let ((frame-to-delete (selected-frame))
	(buffer-to-kill (current-buffer))
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


;; Functions for changing font size
(defun siliusmv/zoom-in ()
  (interactive)
  (let ((x (+ 10 (face-attribute 'default :height))))
    (set-face-attribute 'default nil :height x)))

(defun siliusmv/zoom-out ()
  (interactive)
  (let ((x (- (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height x)))

(defun siliusmv/choose-from-list (prompt var-list &optional var-name)
  (if (not var-name)
      (setq var-name
	    (ivy-read prompt var-list)))
  (nth 1 (assoc var-name var-list)))

(defun siliusmv/kill-this-buffer ()
  "Kill the current buffer without any prompts"
  (interactive)
  (kill-buffer (current-buffer)))

(defun siliusmv/go-to-config ()
  "Go to the emacs config-file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun siliusmv/eval-sexp ()
  "Eval the SEXP you are inside"
  )


;; =========================================================
;; %%%%%%%%%%%%% PACKAGE SPECIFIC SETTINGS %%%%%%%%%%%%%%%%%
;; =========================================================
;; =========================================================
;; Evil-mode
;; =========================================================

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  )


(use-package evil-collection
  :after evil
  :custom (evil-collection-minibuffer-setup t)
  :config
  (evil-collection-init)
  )


;; =========================================================
;; Diminish
;; =========================================================
(use-package diminish)

;; =========================================================
;; General.el - keybindings
;; =========================================================
(use-package general
  :config
  (general-evil-setup t)

  (general-define-key
   :keymaps 'override
   :states '(normal visual insert emacs)
   "M-k" '(scroll-down-command :wk t)
   "M-j" '(scroll-up-command :wk t)
   "M-/" '(counsel-grep-or-swiper :wk "search in buffer")
   "M-?" '(which-key-show-top-level :wk "show all bindings")
   "M-b" '(ivy-switch-buffer :wk "switch buffer")
   "M-p" '(evil-paste-after :wk "copy from clipboard")
   "M-o" '(ace-window :wk "other window")
   "C-M-f" '(toggle-fullscreen :wk "fullscreen")
   "M-n" '(make-frame :wk "new frame")
   "M-s" '(save-buffer :wk "save buffer")
   "M-S" '(save-some-buffers :wk "save all buffers")
   "M-`" '(ns-next-frame :wk "switch frame")
   )

  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "/" '(counsel-grep-or-swiper :wk "search in buffer")
   )

  (general-define-key
   :keymaps 'minibuffer-local-map
   "M-p" '(yank :wk "copy from clipboard")
   )
  

  (general-define-key
   :states 'visual
   :keymaps 'override
   "<tab>" 'indent-for-tab-command
   ;; macOS stuff
   "M-c" 'kill-ring-save
   "M-x" 'kill-region
   )

  ;; Editing commands
  (general-define-key
   :states 'visual
   :keymaps 'override
   :prefix "M-i"
   "" '(:ignore t :wk "insert")
   "ESC" '(:ignore t :wk t)
   "(" '(insert-pair :wk "(")
   "[" '(insert-pair :wk "[")
   "{" '(insert-pair :wk "{")
   "\"" '(insert-pair :wk "\"")
   "\'" '(insert-pair :wk "\'")
   "\$" '(insert-pair :wk "\$")
   "`" '(insert-pair :wk "`")
   ")" '(delete-pair :wk "delete pair")
   )

  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps 'override
   :prefix "M-g"
   "" nil
   ")" '(evil-next-close-paren :wk "next closing parenthesis")
   "(" '(evil-previous-open-paren :wk "prev opening parenthesis")
   "}" '(evil-next-close-brace :wk "next closing brace")
   "{" '(evil-previous-open-brace :wk "prev opening brace")
   "]" '(evil-forward-section-begin :wk "next section")
   "[" '(evil-backward-section-begin :wk "prev section")
   "s" '(flyspell-correct-next :wk "next spelling error")
   "S" '(flyspell-correct-previous :wk "prev spelling error")
   "e" '(flymake-goto-next-error :wk "flymake: next error")
   "E" '(flymake-goto-prev-error :wk "flymake: prev error")
   "f" '(flycheck-next-error :wk "flycheck: next error")
   "F" '(flycheck-previous-error :wk "flycheck: prev error")
   "b" '(next-buffer :wk "next buffer")
   "B" '(previous-buffer :wk "prev buffer")
   "w" '(eyebrowse-next-window-config :wk "next workspace")
   "W" '(eyebrowse-last-window-config :wk "prev workspace")
   )

  ;; Space as leader
  (general-define-key
   :keymaps 'override
   :states '(normal insert visual emacs)
   :prefix "M-SPC"
   ;; Popular keybindings
   "" nil
   "ESC" '(:ignore t :wk t)
   "M-SPC" '(counsel-find-file :wk "find file")
   "," '(counsel-switch-buffer :wk "switch buffer")
   "." '(save-buffer :wk "save buffer")
   "-" '(counsel-grep-or-swiper :wk "search in buffer")
   "g" '(magit-status :wk "git")
   "S" '(save-some-buffers :wk "save all buffers")
   "s" '(counsel-grep-or-swiper :wk "search in buffer")
   "l" '(org-store-link :wk "store org-link")
   "~" '(siliusmv/go-to-config :wk "go home")
   "M-f" '(make-frame :wk "new frame")

   "m" '(:ignore t :wk "mode specific")

   ;; Eval elisp
   "e" '(:ignore t :wk "elisp")
   "e o" '(eval-defun :wk "evaluate outer sexp")
   "e i" '(eval-last-sexp :wk "evaluate inner sexp")

   ;; Buffer keymap
   "b" '(:ignore t :wk "buffers")
   "b k" '(siliusmv/kill-this-buffer :wk "kill buffer")
   "b s" '(save-buffer :wk "save buffer")
   "b SPC" '(persp-switch-to-buffer :wk "switch buffer in persp")
   "b n" '(next-buffer :wk "next buffer")
   "b p" '(previous-buffer :wk "previous buffer")
   "b r" '(rename-buffer :wk "rename buffer")
   "b b" '(ivy-switch-buffer :wk "switch buffer")

   ;; Help keymap
   "h" '(:ignore t :wk "help")
   "h k" '(describe-key :wk "keys")
   "h v" '(describe-variable :wk "variables")
   "h ?" '(help-for-help :wk "all the help-possibilities")
   "h i" '(info :wk "read the manual")
   "h m" '(describe-mode :wk "modes")
   "h f" '(describe-function :wk "functions")

   ;; Files keymap
   "f" '(:ignore t :wk "files")
   "f d" '(dired :wk "open directory")
   "f f" '(counsel-find-file :wk "find files")

   ;; Quit
   "q" '(:ignore t :wk "quit")
   "q f" '(delete-frame :wk "close frame")
   "q w" '(delete-window :wk "close window")
   "q o" '(delete-other-windows :wk "close other windows")
   "q W" '(kill-buffer-and-window :wk "Kill buffer, close window")
   "q F" '(siliusmv/kill-buffer-and-frame :wk "Kill buffer, close frame")

   ;; Search keymap
   "M-s" '(:ignore t :wk "search")
   "M-s b" '(counsel-grep-or-swiper :wk "search in buffer")
   "M-s 0" '(evil-ex-nohighlight :wk "turn off highlight")
   "M-s SPC" '(counsel-grep-or-swiper :wk "search in buffer")
   "M-s d" '(counsel-ag :wk "search in directory")
   "M-s g" '(counsel-git-grep :wk "search in git repository")
   ;; "s p" '(projectile-ripgrep :wk "search in project")

   ;; Project keymap
   "p" '(:keymap projectile-command-map :package projectile :wk "project menu")

   ;; Toggle keymap
   "v" '(:ignore t :wk "change variables")
   "v l" '(siliusmv/nlinum-cycle :wk "toggle line numbers")
   "v d" '(siliusmv/choose-dictionary :wk "spell-check dictionary")
   "v t" '(siliusmv/choose-theme :wk "theme")
   "v s" '(flyspell-mode :wk "toggle spelling")
   "v F" '(flycheck-mode :wk "toggle flycheck")
   "v f" '(:ignore t :wk "font size")
   "v f +" '(siliusmv/zoom-in :wk "enlarge")
   "v f -" '(siliusmv/zoom-out :wk "decrease")

   ;; Window keymap
   "w" '(:ignore t :wk "window")
   "w +" '(evil-window-increase-height :wk "increase height")
   "w -" '(evil-window-decrease-height :wk "decrease height")
   "w =" '(balance-windows :wk "balance windows")
   "w <" '(evil-window-increase-width :wk "increase width")
   "w >" '(evil-window-decrease-width :wk "decrease width")
   "w h" '(evil-window-left :wk t)
   "w j" '(evil-window-down :wk t)
   "w k" '(evil-window-up :wk t)
   "w l" '(evil-window-right :wk t)
   "w H" '(evil-window-move-far-left :wk t)
   "w J" '(evil-window-move-far-down :wk t)
   "w K" '(evil-window-move-far-up :wk t)
   "w L" '(evil-window-move-far-right :wk t)
   "w d" '(delete-window :wk "delete window")
   "w f" '(delete-other-windows :wk "focus on window")
   "w v" '(split-window-right :wk "split vertical")
   "w s" '(split-window-below :wk "split")
   "w o" '(ace-window :wk "jump to other window")

   ;; "Open programs" - keymap
   "o" '(:ignore t :wk "open program")
   "o t" '(multi-term :wk "terminal")
   "o r" '(run-ess-r :wk "R session")
   "o f" '(make-frame-command :wk "frame")
   "o d" '(dired :wk "dired")

   ;; "Workspaces"
   "M-w" '(:ignore t :wk "workspaces")
   "M-w n" '(eyebrowse-create-window-config :wk "new")
   "M-w k" '(eyebrowse-next-window-config :wk "next")
   "M-w j" '(eyebrowse-prev-window-config :wk "prev")
   "M-w s" '(eyebrowse-switch-to-window-config :wk "switch to other")
   "M-w r" '(eyebrowse-rename-window-config :wk "rename")
   "M-w d" '(eyebrowse-close-window-config :wk "close current")
   )
  )

;;; ========================================================
;;; Dired stuff
;;; ========================================================
(defun dired-hide-dotfiles ()
  "Hides all dotfiles in a dired-buffer"
  (interactive)
  (dired-mark-files-regexp "^\\.")
  (dired-do-kill-lines)
  )

(general-define-key
 :keymaps 'dired-mode-map
 :states 'normal
 "l" 'dired-find-file
 "h" 'dired-up-directory
 "M-SPC m h" '(dired-hide-dotfiles :wk "hide dotfiles")
 "M-SPC m c" '(dired-do-copy :wk "copy")
 "M-SPC m m" '(dired-do-rename :wk "move")
 "M-SPC m d" '(dired-do-delete :wk "delete")
 "M-SPC m s" '(dired-do-symlink :wk "symlink")
 )



;; =========================================================
;; Language servers
;; =========================================================
(use-package eglot)


(use-package flymake
  :init
  ;; Change FlyMake to FM on the mode-line
  (defun flymake--transform-mode-line-format (ret)
    "Change the output of `flymake--mode-line-format'."
    (setf (seq-elt (car ret) 1) " FM")
    ret)
  (advice-add #'flymake--mode-line-format
	      :filter-return #'flymake--transform-mode-line-format)
  )

;; =========================================================
;; Relative nlinum mode
;; =========================================================
(use-package nlinum-relative

  :diminish nlinum-relative-mode
  :config
  (setq nlinum-highlight-current-line t)
  ;(nlinum-relative-setup-evil)             ;; setup for evil
  (setq nlinum-relative-redisplay-delay 0)      ;; delay
  (setq nlinum-relative-current-symbol "")
  (add-hook 'text-mode-hook 'nlinum-relative-mode)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)

  (defun siliusmv/nlinum-off ()
    (nlinum-relative-mode 0)
    (nlinum-mode 0))

  (defun siliusmv/nlinum-cycle ()
    "Toggle between nlinum, nlinum-relative and no line numbering"
    (interactive)
    (if (and (bound-and-true-p nlinum-mode)
	     (bound-and-true-p nlinum-relative-mode))
	(siliusmv/nlinum-off)
      (if (bound-and-true-p nlinum-mode)
	  (nlinum-relative-mode)
	(nlinum-mode)))
    )
  )

;; =========================================================
;; Email
;; =========================================================
(if mu4e-p
    (use-package mu4e
      :after evil
      :commands (mu4e)
      :general
      (:keymaps '(mu4e-headers-mode-map mu4e-view-mode-map mu4e-main-mode-map)
       :states '(motion normal insert visual emacs)
       :prefix "M-SPC m"
       "g" '(siliusmv/gmail-firefox :wk "gmail")
       "o" '(siliusmv/outlook-firefox :wk "outlook")
       )
      (:keymaps 'override
       :prefix "M-SPC"
       "M-o m" '(mu4e :wk "email")
       )
      :init

      (defun siliusmv/gmail-firefox ()
	"Open gmail in firefox"
	(interactive)
	(browse-url-firefox "https://gmail.com" t)
	)

      (defun siliusmv/outlook-firefox ()
	"Open outlook in firefox"
	(interactive)
	(browse-url-firefox "https://mail.ntnu.no/owa/silius.m.vandeskog@ntnu.no" t)
	)

      (setq-default
       message-send-mail-function 'message-send-mail-with-sendmail
       sendmail-program "/usr/bin/msmtp"
       user-full-name "Silius Mortensønn Vandeskog"
       user-mail-address "siliusv@gmail.com"
       )

      ;; Set mu4e as default mail agent
      (setq mail-user-agent 'mu4e-user-agent)

      :config
      (defvar base-signature
	(concat
	 "Silius M. Vandeskog\n"
	 "Phone: +47 93 68 49 45"))
      (setq-default
       mu4e-maildir "~/.mail"
       mu4e-sent-folder "/gmail/sent"
       mu4e-drafts-folder "/gmail/drafts"
       mu4e-trash-folder "/gmail/trash"
       mu4e-refile-folder "/gmail/all_mail"
       mu4e-compose-signature base-signature
       )

      ;; https://www.djcbsoftware.nl/code/mu/mu4e/Reading-messages.html
      (add-to-list 'mu4e-view-actions
		   '("ViewInBrowser" . mu4e-action-view-in-browser) t)

					; http://www.djcbsoftware.nl/code/mu/mu4e/Viewing-images-inline.html#Viewing-images-inline
					; enable inline images
      (setq mu4e-view-show-images t)
      ;; use imagemagick, if available
      (when (fboundp 'imagemagick-register-types)
	(imagemagick-register-types))


      ;; HTML settings
      ;; (require 'shr)
      ;; (defun shr-render-current-buffer ()
      ;;   (shr-render-region (point-min) (point-max)))
      ;; (setq mu4e-html2text-command 'shr-render-current-buffer)

      ;;(setq mu4e-html2text-command "html2text -utf8 -width 72")
      ;;(setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
      ;;(setq mu4e-html2text-command "pandoc -f html --pdf-engine=pdflatex")
      (setq mu4e-html2text-command "w3m -dump -T text/html -cols 72 -o display_link_number=true -o auto_image=false -o display_image=false -o ignore_null_img_alt=true")

      (setq browse-url-browser-function 'browse-url-generic)
      (setq browse-url-generic-program "qutebrowser")
      ;; (setq browse-url-generic-args '("--target window"))

      (setq mu4e-compose-dont-reply-to-self t)

      ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/?fbclid=IwAR2Udao8s5OrwCSPs9_NgHI1fjxdJJXwZhnaMWKuMhTvcdZxaNzINCi6tq4
      (setq mu4e-contexts
	    `( ,(make-mu4e-context
		 :name "gmail"
		 :match-func (lambda (msg) (when msg
					     (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
		 :vars '((user-mail-address . "siliusv@gmail.com")
			 (mu4e-sent-folder . "/gmail/sent")
			 (mu4e-drafts-folder . "/gmail/drafts")
			 (mu4e-trash-folder . "/gmail/trash")
			 (mu4e-refile-folder . "/gmail/all_mail")
			 (mu4e-compose-signature . (concat
						    base-signature
						    "\nEmail: siliusv@gmail.com"))
			 ))
	       ,(make-mu4e-context
		 :name "work"
		 :match-func (lambda (msg) (when msg
					     (string-prefix-p "/work" (mu4e-message-field msg :maildir))))
		 :vars '(
			 (user-mail-address ."siliusmv@stud.ntnu.no")
			 (mu4e-sent-folder . "/work/sent")
			 (mu4e-drafts-folder . "/work/drafts")
			 (mu4e-trash-folder . "/work/trash")
			 (mu4e-refile-folder . "/work/Archive1")
			 (mu4e-compose-signature .
						 (concat
						  base-signature
						  "\nEmail: "
						  "siliusmv@stud.ntnu.no"))
			 ))
	       ))

      (add-to-list 'mu4e-bookmarks
		   (make-mu4e-bookmark
		    :name "All Inboxes"
		    :query "maildir:/work/inbox OR maildir:/gmail/inbox"
		    :key ?i))

      

      ;; Why would I want to leave my message open after I've sent it?
      (setq message-kill-buffer-on-exit t)
      ;; Don't ask for a 'context' upon opening mu4e
      (setq mu4e-context-policy 'pick-first)
      ;; Don't ask to quit... why is this the default?
      (setq mu4e-confirm-quit nil)

      
      (setq-default mu4e-get-mail-command "mbsync -a"
		    mu4e-update-interval nil ;; This is handled by another script
		    mu4e-change-filenames-when-moving t)

      ;; Don't get duplicate mails. gmail takes care of copying mails
      (setq mu4e-sent-messages-behavior 'delete)
      
      ;; Shortcuts
      (setq mu4e-maildir-shortcuts 
	    '(("/gmail/inbox" . ?g)
	      ("/work/inbox" . ?w))
	    )
      ;; This is needed to allow msmtp to do its magic:
      (setq message-sendmail-f-is-evil 't)
      ;;need to tell msmtp which account we're using
      (setq message-sendmail-extra-arguments '("--read-envelope-from"))
      ) 
  )



;; =========================================================
;; Text navigation
;; =========================================================

(use-package avy

  :general
  (:states '(normal visual)
   "s" 'avy-goto-char-timer
   )
  :config
  (setq avy-style 'at-full
	avy-all-windows t
	avy-timeout-seconds 0.2)
  )

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ; letters for switching window
  (setq aw-scope 'frame) ;; Only switch windows in the focused frame
  (setq aw-background nil) ; Don't remove colour around the letters
  )

;; =========================================================
;; Company
;; =========================================================
(use-package company
  :delight
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config

  (general-define-key
   :keymaps 'company-active-map
   "<tab>" 'company-complete-common-or-cycle
   "<backtab>" 'company-select-previous
   "<return>" '(:ignore t)
   "M-j" 'company-select-next
   "M-k" 'company-select-previous
   "M-s" 'company-search-candidates
   "M-J" 'company-next-page
   "M-K" 'company-previous-page
   "M-l" 'company-complete-common

   "M-SPC m o" '(company-other-backend :wk "other backend")
   "M-SPC m d" '(company-diag :wk "diagnosis")
   "M-SPC m c" '(counsel-company :wk "counsel-company")
   "M-SPC m h" '(company-doc-buffer :wk "show documentation")
   )
  
  (general-define-key
   :states 'insert
   :keymaps 'company-mode-map
   "M-c" 'company-other-backend
   )

  ;; set default `company-backends'
  (setq company-backends
	'(company-capf
	  company-files          ; files & directory
	  (company-keywords       ; keywords (I don't know what this does...)
	   company-yasnippet :separate)
	  (company-abbrev company-dabbrev :separate)
	  ))


  ;; Behavoiur of completion pop-up
  (setq company-selection-wrap-around t
	company-tooltip-align-annotations t
	company-idle-delay 0.1
	company-minimum-prefix-length 1
	company-tooltip-limit 10)

  ;; Settings for backends
  (setq company-dabbrev-downcase nil
	company-dabbrev-code-ignore-case t
	company-dabbrev-ignore-case t)
  
  )

(use-package company-statistics
  :config
  (add-hook 'after-init-hook 'company-statistics-mode))



;; =========================================================
;; ESS (Emacs Speaks Statistics)
;; =========================================================
(use-package ess
  :commands (run-ess-r)
  :defer 5
  :general
  (:keymaps '(ess-r-mode-map inferior-ess-mode-map)
	    :prefix "M-SPC m"
	    :states '(motion normal insert visual emacs)
	    "r" '(run-ess-r :wk "Open new R session")
	    "s" '(ess-switch-process :wk "Switch R session")
	    )
  (:keymaps '(ess-r-mode-map inferior-ess-mode-map)
	    :states '(motion normal insert visual emacs)
	    "M-e" 'ess-eval-region-or-line-visibly-and-step
	    "M-RET" 'ess-eval-region-or-function-or-paragraph-and-step
	    )
  :diminish
  ((ess-r-package-mode . "")
   (eldoc-mode . ""))
  :init

  ;; Enable sweaving directly within the AUCTeX ecosystem.
  (setq-default ess-swv-plug-into-AUCTeX-p t)

  ;; Automagically delete trailing whitespace when saving R script
  ;; files. One can add other commands in the ess-mode-hook below.
  (add-hook 'ess-mode-hook
	    '(lambda()
	       (add-hook 'write-contents-functions
			 (lambda ()
			   (ess-nuke-trailing-whitespace)))
	       (setq ess-nuke-trailing-whitespace-p t)))

  (require 'ess-site)
  :config
  (add-hook 'inferior-ess-r-mode-hook
	    (lambda ()
	      (nlinum-relative-mode -1)
	      (electric-pair-local-mode -1)))

  (setq ess-inject-source nil
	ess-r-package-auto-enable-namespaced-evaluation nil ;; Not use namespace
	ess-eval-visibly nil ;; Something about echoing code
	ess-use-eldoc 'script-only ;; Try to speed up iess[r]-buffer
	inferior-R-args " --no-restore-history --no-save -q" ;; Startup-args
	ess-history-file nil ;; Do not save history of each session
	ansi-color-for-comint-mode 'filter ;; Remove annoying colors
	ess-local-process-name "R"
	ess-eval-visibly-p 'nowait ;; no waiting while ess evaluating
	)
  ;; start help in normal mode
  (evil-set-initial-state 'ess-help-mode 'normal) 

  ;; Enable better search history
  (eval-after-load "comint"
    '(progn
       (define-key comint-mode-map [up]
	 'comint-previous-matching-input-from-input)
       (define-key comint-mode-map [down]
	 'comint-next-matching-input-from-input)
       ;; also recommended for ESS use --
       (setq comint-move-point-for-output 'others)
       ;; somewhat extreme, almost disabling writing in
       ;; *R*, *shell* buffers above prompt:
       (setq comint-scroll-to-bottom-on-input 'this)))

  (defun my-ess-company-function ()
    (set (make-local-variable 'company-backends)
	 '( (company-capf ; Works together with eglot
	     company-files
	     company-R-args
	     company-R-objects
	     company-R-library
	     company-dabbrev-code :separate)
	    (company-R-args
	     company-R-objects
	     company-R-library :separate)
	    company-files
	    )))
  (setq ess-use-company nil) ; Don't let ESS decide backends
  
					; Company settings for ess
  (add-hook 'ess-mode-hook 'my-ess-company-function)
  (add-hook 'inferior-ess-mode-hook 'my-ess-company-function)

					; Eglot stuff
  (add-hook 'ess-mode-hook 'eglot-ensure)
  (add-hook 'inferior-ess-mode-hook 'eglot-ensure)

  ;; ;; Try to limit max buffer size
  ;; ;;(add-hook 'inferior-ess-mode-hook 'comint-truncate-buffer)
  ;; (add-hook 'inferior-ess-mode-hook
  ;; 	    (lambda ()
  ;; 	      (setq comint-buffer-maximum-size 1000)
  ;; 	      (run-with-timer 300 300 'comint-truncate-buffer)
  ;; 	      ))

  )


;; =========================================================
;; Themes
;; =========================================================
(use-package doom-themes
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t)
  ;; doom-one-comment-bg t
  ;; doom-one-light-comment-bg t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  ;; (load-theme 'doom-one t)

  (defvar siliusmv/my-themes
    (list
     '("dark" doom-one)
     '("light" doom-one-light)))

  (defun siliusmv/choose-theme (&optional theme-short)
    (interactive)
    (let ((theme
	   (siliusmv/choose-from-list "Select theme: " siliusmv/my-themes theme-short)))
      (load-theme theme t)))

  (siliusmv/choose-theme init-theme)
  )

 ;; =========================================================
 ;; Wokspaces
 ;; =========================================================
 
 (use-package eyebrowse
   :init
   (eyebrowse-mode)
   :general
   (:keymaps 'override
    "M-1" '(eyebrowse-switch-to-window-config-1 :wk "workspace 1")
    "M-2" '(eyebrowse-switch-to-window-config-2 :wk "workspace 2")
    "M-3" '(eyebrowse-switch-to-window-config-3 :wk "workspace 3")
    "M-4" '(eyebrowse-switch-to-window-config-4 :wk "workspace 4")
    "M-5" '(eyebrowse-switch-to-window-config-5 :wk "workspace 5")
    "M-6" '(eyebrowse-switch-to-window-config-6 :wk "workspace 6")
    "M-7" '(eyebrowse-switch-to-window-config-7 :wk "workspace 7")
    "M-8" '(eyebrowse-switch-to-window-config-8 :wk "workspace 8")
    "M-9" '(eyebrowse-switch-to-window-config-9 :wk "workspace 9")

    "M-w" '(:ignore t :wk "workspace cycling")
    "M-w k" '(eyebrowse-next-window-config :wk "next")
    "M-w j" '(eyebrowse-prev-window-config :wk "prev")
    )
   :config
   (setq eyebrowse-new-workspace t ; Start new workspace with scratch
	 eyebrowse-wrap-around t ; Go from last to first worskspace when cycling
	 )
   )
 
 
 ;;(use-package persp-mode
 ;;  :init
 ;;  (persp-mode 1)
 ;;  :config
 ;;  ;; This was advised from https://github.com/Bad-ptr/persp-mode.el
 ;;  (setq wg-morph-on nil)
 ;;  (setq persp-autokill-buffer-on-remove 'kill-weak)
 ;;
 ;;  (setq persp-nil-hidden t ;; Hide nil-perspecive
 ;;	persp-auto-save-opt (if noninteractive 0 1)
 ;;	persp-nil-name "main"
 ;;	persp-auto-save-fname "autosave"
 ;;	persp-auto-resume-time -1 ;; Do not autoresume
 ;;	persp-remove-buffers-from-nil-persp-behaviour nil
 ;;	persp-init-frame-behaviour nil ;; Open scratch for new frames
 ;;	)
 ;;  )
 
 ;; =========================================================
 ;; Tramp
 ;; =========================================================
 (use-package tramp
 
   :config
   (setq tramp-default-method "ssh"))
 
 ;; =========================================================
 ;; Magit
 ;; =========================================================
 (use-package magit
   :commands (magit-status)
   :config
   ;; Possible performance fix
   (setq auto-revert-buffer-list-filter
 	'magit-auto-revert-repository-buffer-p)
 
   (use-package evil-magit) ;; Evil-movements in magit
   )
 
 
 ;; =========================================================
 ;; Rainbow delimiters
 ;; =========================================================
 (use-package rainbow-delimiters
   :hook (prog-mode . rainbow-delimiters-mode)
   :defer 5
   )
 
 
 ;; =========================================================
 ;; Jump to function definition
 ;; =========================================================
 (use-package imenu-anywhere)
 
 (use-package dumb-jump
   :hook (prog-mode . dumb-jump-mode)
   :general
   (:states '(normal insert emacs)
    :prefix "M-SPC"
    "" nil
    "j" '(:ignore t :wk "jump to text")
    "j d" '(dumb-jump-go :wk "dumb-jump")
    "j i" '(ivy-imenu-anywhere :wk "imenu")
    )
   )
 
;; =========================================================
;; Ivy++
;; =========================================================
(use-package ivy
  :diminish ivy-mode
  :general
   (:keymaps 'ivy-minibuffer-map
   "<tab>" 'ivy-next-line
   "<backtab>" 'ivy-previous-line
   "M-j" 'ivy-next-line
   "M-k" 'ivy-previous-line
   "M-l" 'ivy-alt-done
   "M-s" 'ivy-avy
   "M-h" 'ivy-backward-kill-word
   "M-K" 'ivy-scroll-down-command
   "M-J" 'ivy-scroll-up-command
   "M-v" 'yank ; For pasting passwords into the minibuffer in tramp
   )
  :init
  ;; The default search is ivy--regex-plus
  (setq ivy-re-builders-alist
	'((t . ivy--regex-ignore-order))) ;; Regexps can interchange order

  (setq ivy-height 20)

  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t) ;; Proposed from the Ivy wiki
  (setq ivy-count-format "(%d/%d) ") ;; Proposed from the Ivy wiki
  )

(use-package swiper
  :config
  :commands (swiper)
  )

(use-package counsel
  :general
  ( 
   "M-x" 'counsel-M-x
   "C-x C-f" 'counsel-find-file
   "<f1> f" 'counsel-describe-function
   "<f1> v" 'counsel-describe-variable
   "<f1> l" 'counsel-find-library
   "<f2> i" 'counsel-info-lookup-symbol
   "<f2> u" 'counsel-unicode-char
   )


  ;; ;; Ivy-based interface to shell and system tools
  ;; (global-set-key (kbd "C-c c") 'counsel-compile)
  ;; (global-set-key (kbd "C-c g") 'counsel-git)
  ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c k") 'counsel-ag)
  ;; (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  )

;; Show last used functions in M-x
(use-package amx)


;; =========================================================
;; Flycheck (linting)
;; =========================================================
(use-package flycheck)


;; =========================================================
;; LaTeX-stuff (AuCTeX, refTeX and more)
;; =========================================================
(use-package auctex
  ;; :straight (auctex :host github
  ;;                   :repo "emacsmirror/auctex"
  ;;                   :files (:defaults (:exclude "*.el.in")))
  ;; :hook 
  ;; (LaTeX-mode . 'LaTeX-math-mode)
  :general
  (:keymaps 'TeX-mode-map
   :prefix "M-SPC m"
   "v" '(TeX-view :wk "view pdf")
   "c" '(TeX-command-master :wk "compile document")
   "t" '(reftex-toc :wk "navigate document")
   "r" '(reftex-toc-Rescan :wk "refresh reftex")
   "e" '(TeX-next-error :wk "compilation errors")
   "f" '(LaTeX-fill-buffer :wk "fill buffer")
   "M-v" '(:ignore t :wk "change variables")
   "M-v f" '(siliusmv/toggle-tex-fold :wk "folding")
   "M-v v" '(siliusmv/choose-latex-pdf-viewer :wk "PDF viewer")
  )

  :init

  (add-hook 'LaTeX-mode-hook 'latex-math-mode)

  ;; Completion for latex macros
  (setq 
   TeX-auto-global "~/.emacs.d/auctex/auto-global"
   TeX-auto-regexp-list 'TeX-auto-full-regexp-list)

  

  ;;; Functions for changing PDF viewers
  (defvar siliusmv/pdf-viewers
    (list
     '("evince" "Evince")
     '("pdf-tools" "PDF Tools")
     '("zathura" "Zathura2")
     '("preview" "Preview.app")))

  (defun siliusmv/choose-latex-pdf-viewer (&optional viewer-name)
    "Change PDF viewer for latex"
    (interactive)
    (let ((viewer
	   (siliusmv/choose-from-list
	    "Select PDF viewer: "
	    siliusmv/pdf-viewers
	    viewer-name)))
      (delete `(output-pdf ,viewer) TeX-view-program-selection)
      (setq TeX-view-program-selection
      	    (cons `(output-pdf ,viewer) TeX-view-program-selection))))


  ;;; Functions for doing text-folding

  (defvar siliusmv/toggle-state "unfolded")
  (defun siliusmv/toggle-tex-fold ()
    "Toggle between folded and unfolded buffers.
   If TeX-fold-mode is not activated, first activate it."
    (interactive)
    (if (not (bound-and-true-p TeX-fold-mode))
  	(TeX-fold-mode))
    (if (equal siliusmv/toggle-state "folded")
  	(progn
  	  (TeX-fold-clearout-buffer)
  	  (setq siliusmv/toggle-state "unfolded"))
      (progn
  	(TeX-fold-buffer)
  	(setq siliusmv/toggle-state "folded"))))


  (add-hook 'TeX-mode-hook
	    (lambda ()
	      ;; Add backwards search to zathura
	      ;; https://www.emacswiki.org/emacs/AUCTeX#toc23
	      (add-to-list 'TeX-view-program-list
			   '("Zathura2"
			     ("zathura %o"
			      (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=my-gui-server --no-wait +%{line} %{input}\""))
			     "zathura"))

	      ;; Add PDF Tools as a possible viewer
	      (unless (assoc "PDF Tools" TeX-view-program-list-builtin)
		(add-to-list 'TeX-view-program-list-builtin
			     '("PDF Tools" TeX-pdf-tools-sync-view)))))

  :config
  (siliusmv/choose-latex-pdf-viewer "zathura")

  (setq TeX-complete-expert-commands t) ; Adds more commands for completion
  (setq Tex-auto-save t) ;; Parsing on save
  (setq TeX-parse-self t) ;; Parsing on load (scan file for macros)
  (setq-default TeX-master nil) ;; Allow multi-file documents
  (setq-default TeX-PDF-mode t)

  ;; automatically insert braces after sub/superscript in math mode
  (setq TeX-electric-sub-and-superscript t)

  ;; Syntax highlighting
  ;; (not sure if this is correct way to activate)
  (global-font-lock-mode t) 

  

  (setq TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode)
  (setq TeX-source-correlate-start-server t)

  )

(use-package reftex
  :diminish reftex-mode
  :config
  (add-hook 'LaTeX-mode-hook
  	    (lambda () (reftex-mode 1)))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTEX t)
  )

(use-package auctex-latexmk
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (setq TeX-command-default "LatexMk")
			       (setq TeX-command-force "LatexMk")
			       (TeX-source-correlate-mode t)))
  :config
  (auctex-latexmk-setup)
  ;; (add-hook 'LaTeX-mode-hook (lambda () (auctex-latexmk-setup)))
  )


(use-package company-reftex)
(use-package company-auctex
  :config

  (defun my-latex-company-function ()
    (set (make-local-variable 'company-backends)
	 '(
	   (company-reftex-labels
	    company-reftex-citations
	    company-auctex-macros
	    company-auctex-bibs
	    company-auctex-environments
	    company-auctex-symbols :separate)
	   company-capf
	   (company-files
	    company-dabbrev-code :separate)
	   company-abbrev
	   company-dabbrev
	   ))
    (company-auctex-init))

  (add-hook 'LaTeX-mode-hook 'my-latex-company-function)
  
  )


;; =========================================================
;; Auto-fill texts
;; =========================================================
(setq-default fill-column 75)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode 'auto-fill-mode)
(diminish 'auto-fill-function)

(diminish 'undo-tree-mode)
(diminish 'auto-fill-mode)
(diminish 'auto-revert-mode)
;; =========================================================
;; Dictionary (flyspell)
;; =========================================================
(use-package flyspell
  :diminish flyspell-mode
  :config

  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)

  ;; Flyspell for comments and strings in prog-mode
  (add-hook 'prog-mode 'flyspell-prog-mode)

  ;; ;; Not sure that this works
  ;; (setq-default ispell-program-name "hunspell")
  ;; (setq ispell-really-hunspell t)
  (setq-default ispell-program-name "/usr/local/bin/aspell")

  (defvar siliusmv/my-dictionaries
    (list
     '("british" "en_GB")
     '("norwegian" "norsk7-tex")))

  (defun siliusmv/choose-dictionary (&optional dict-name)
    (interactive)
    (let ((dict
	   (siliusmv/choose-from-list "Select dictionary: " siliusmv/my-dictionaries dict-name)))
      (ispell-change-dictionary dict)))


  (siliusmv/choose-dictionary init-dict)


  (use-package flyspell-correct-ivy
    ;; :bind (:map flyspell-mode-map
    ;; ("C-c $" . flyspell-correct-word-generic)))
    )
  )


;; ;; =========================================================
;; ;; PDF Tools
;; ;; =========================================================
(use-package tablist) ;; Apparently necessary for PDF Tools

(use-package pdf-tools
  :config

  (setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")

  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")

  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (general-define-key
   :keymaps 'pdf-view-mode-map
   :states 'normal
    "f" 'pdf-links-action-perform
    "J" 'pdf-view-next-page-command
    "K" 'pdf-view-previous-page-command
   )

  ;; This does not seem to work
  (general-define-key
   :keymaps 'pdf-view-mode-map
    "M-j" 'pdf-view-next-page-command
    "M-k" 'pdf-view-previous-page-command
   )

  (general-define-key
   :keymaps 'pdf-view-mode-map
   :prefix "M-SPC m"
   "t" '(pdf-outline :wk "toc") 

   "/" '(isearch-forward :wk "search in buffer")

   "a" '(:ignore t :wk "annotations")
   "a t" '(pdf-annot-add-text-annotation :wk "text")
   "a m" '(pdf-annot-add-markup-annotation :wk "markup")
   "a d" '(pdf-annot-delete :wk "delete")
   "a l" '(pdf-annot-list-annotations :wk "list annotations")
   )

  ;; Stop the annoying blinking in the pdf
  (add-hook 'pdf-view-mode-hook
	    (lambda ()
	      (blink-cursor-mode -1)))
  )


;; =========================================================
;; which-key
;; =========================================================
;; Display a popup-buffer with the available key-combinations
;; whenever a keymap is pressed
(use-package which-key

  :defer 5
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-sort-order 'which-key-local-then-key-order)
  (which-key-setup-side-window-bottom)
  (setq which-key-side-window-max-height 0.5)
  )



;; =========================================================
;; Project management
;; =========================================================
(use-package projectile

  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  )

;; Install ag or ripgrep!!!!


;; =========================================================
;; Terminal
;; =========================================================
(use-package multi-term
  :commands (multi-term)
  :general
  (:keymaps 'term-mode-map
   :states '(motion normal insert visual emacs)
   :prefix "M-SPC m"

   "s" '(:ignore t :wk "send signal to shell")
   "s q" '(term-quit-subjob :wk "quit")
   "s k" '(term-kill-subjob :wk "kill")
   "s s" '(term-stop-subjob :wk "stop")
   "s c" '(term-continue-subjob :wk "continue")
   "s i" '(term-interrupt-subjob :wk "interupt")

   "b" '(:ignore t :wk "terminal buffer commands")
   "b n" '(multi-term-next :wk "next buffer")
   "b p" '(multi-term-prev :wk "previous buffer")
   "b k" '(kill-this-buffer :wk "kill buffer")

   "q" '(siliusmv/kill-buffer-and-frame :wk "kill buffer, close frame")
   )
  :config
  (setq multi-term-program "/bin/bash")
  )


;; =========================================================
;; Pairing of parentheses
;; =========================================================
(use-package elec-pair
  :init
  :defer 5
  :config
  (electric-pair-mode 1)
  ;; pairing of parentheses and other symbols
  (setq-default electric-pair-pairs
		'((?\" . ?\")
		  (?\{ . ?\})
		  (?\( . ?\))
		  (?\[ . ?\])
		  ))
  ;; ;; Do not complete if right in front of a word
  ;; (setq-default electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (show-paren-mode) ;; Highlight matching parenthesis
  )


;; =========================================================
;; Org-mode
;; =========================================================
;;(use-package org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (R . t)
   (latex . t)
   (shell . t)
   (C . t)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (nlinum-mode -1)))

(defun siliusmv/org-cycle-current-headline ()
  "Cycle the current headline. Taken from
https://stackoverflow.com/questions/8607656/emacs-org-mode-how-to-fold-block-without-going-to-block-header"
  (interactive)
  (org-cycle-internal-local))


(general-define-key
 :keymaps 'org-mode-map
 :states '(motion normal insert visual)
 "<tab>" 'org-cycle
 "C-<tab>" 'siliusmv/org-cycle-current-headline
 )

(general-define-key
 :keymaps 'org-mode-map
 :states '(normal visual insert)
 "g" nil
 "g s" '(avy-org-goto-heading-timer :wk "avy heading")
 "g j" '(org-next-visible-heading :wk "next heading")
 "g k" '(org-previous-visible-heading :wk "prev heading")
 "g h" '(outline-up-heading :wk "up one heading")
 "g M-j" '(org-next-block :wk "next block")
 "g M-k" '(org-previous-block :wk "next block")
 )

(general-define-key
 :keymaps 'org-mode-map
 "C-M-j" '(org-next-visible-heading :wk "next heading")
 "C-M-k" '(org-prev-visible-heading :wk "prev heading")
 )

(setq org-export-use-babel nil)

;; Necessary for exporting org to html
(use-package htmlize)

;; Prettify pdf exports
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Do asynchronous org-babel calls in an R session
(use-package ob-session-async
  :straight
  (:type git
   :host github
   :repo "jackkamm/ob-session-async")
  :config
  (require 'ob-session-async-R)
  )


(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;; =========================================================================
;; Modeline
;; =========================================================================


(use-package minions
  :config (minions-mode 1))


(use-package all-the-icons :config (setq all-the-icons-scale-factor 1.0))

(use-package fancy-battery
  :config
  (fancy-battery-mode))

(use-package doom-modeline
  :config
  ;; How tall the mode-line should be (only respected in GUI Emacs).
  (setq doom-modeline-height 25)

  ;; How wide the mode-line bar should be (only respected in GUI Emacs).
  (setq doom-modeline-bar-width 3)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are expereicing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'buffer-name)

  ;; Whether show `all-the-icons' or not (if nil nothing will be showed).
  (setq doom-modeline-icon t)

  ;; Whether show the icon for major mode. It respects `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)

  ;; Display color icons for `major-mode'. It respects `all-the-icons-color-icons'.
  (setq doom-modeline-major-mode-color-icon nil)

  ;; If non-nil, only display one number for checker information if applicable.
  (setq doom-modeline-checker-simple-format t)
  
  ;; Whether display `lsp' state or not. Non-nil to display in mode-line.
  (setq doom-modeline-lsp t)

  ;; Whether display environment version or not
  (setq doom-modeline-env-version t)

  ;; Only show true name for symlinks
  (setq find-file-visit-truename t)

  ;; Display time
  (setq display-time-format
	(format-time-string "%H:%M"))


  (doom-modeline-def-segment my-time
    (let* ((hour (string-to-number (format-time-string "%I")))
	   (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.3 :v-adjust 0.0)))
      (concat
       (propertize (format-time-string " %H:%M ") 'face `(:height 0.9))
       (propertize (format "%s " icon) 'face `(:height 1.0 :family ,(all-the-icons-wicon-family)) 'display '(raise -0.0)))))
  
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
    '(objed-state misc-info persp-name fancy-battery my-time grip irc mu4e github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))

  (doom-modeline-def-modeline 'minimal
    '(bar matches buffer-info-simple)
    '(media-info major-mode))

  (doom-modeline-def-modeline 'special
    '(bar window-number modals matches buffer-info buffer-position parrot selection-info)
    '(objed-state misc-info fancy-battery my-time irc-buffers debug lsp minor-modes input-method indent-info buffer-encoding major-mode process checker))

  (doom-modeline-def-modeline 'project
    '(bar window-number buffer-default-directory)
    '(misc-info fancy-battery my-time mu4e github debug major-mode process))

  (doom-modeline-def-modeline 'package
    '(bar window-number package)
    '(misc-info major-mode process))

  (doom-modeline-def-modeline 'info
    '(bar window-number buffer-info info-nodes buffer-position parrot selection-info)
    '(misc-info buffer-encoding major-mode))

  (doom-modeline-def-modeline 'media
    '(bar window-number buffer-size buffer-info)
    '(misc-info media-info major-mode process vcs))

  (doom-modeline-def-modeline 'pdf
    '(bar window-number buffer-size buffer-info pdf-pages)
    '(misc-info major-mode process vcs))

  (doom-modeline-def-modeline 'helm
    '(bar helm-buffer-id helm-number helm-follow helm-prefix-argument)
    '(helm-help))

  (doom-modeline-def-modeline 'timemachine
    '(bar window-number matches git-timemachine buffer-position parrot selection-info)
    '(misc-info fancy-battery my-time mu4e github debug minor-modes indent-info buffer-encoding major-mode))


  (defun doom-modeline-set-main-modeline (&optional default)
    "Set main mode-line.
If DEFAULT is non-nil, set the default mode-line for all buffers."
    (doom-modeline-set-modeline 'main default))

  (defun doom-modeline-set-minimal-modeline ()
    "Set minimal mode-line."
    (doom-modeline-set-modeline 'minimal))

  (defun doom-modeline-set-special-modeline ()
    "Set sepcial mode-line."
    (doom-modeline-set-modeline 'special))

  (defun doom-modeline-set-project-modeline ()
    "Set project mode-line."
    (doom-modeline-set-modeline 'project))

  (defun doom-modeline-set-info-modeline ()
    "Set Info mode-line."
    (doom-modeline-set-modeline 'info))

  (defun doom-modeline-set-package-modeline ()
    "Set package mode-line."
    (doom-modeline-set-modeline 'package))

  (defun doom-modeline-set-media-modeline ()
    "Set media mode-line."
    (doom-modeline-set-modeline 'media))

  (defun doom-modeline-set-pdf-modeline ()
    "Set pdf mode-line."
    (doom-modeline-set-modeline 'pdf))

  (defun doom-modeline-set-helm-modeline (&rest _)
    "Set helm mode-line."
    (doom-modeline-set-modeline 'helm))

  (defun doom-modeline-set-timemachine-modeline (&rest _)
    "Set timemachine mode-line."
    (doom-modeline-set-modeline 'timemachine))

  (doom-modeline-mode)
  )



;; =========================================================
;; Other stuff
;; =========================================================

;; Restore last emacs session
(desktop-save-mode 1)





(require 'iso-transl)




;; =========================================================
;; Mandatory stuff
;; =========================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes nil)
 '(ess-style (quote RStudio))
 '(evil-collection-minibuffer-setup t t)
 '(evil-search-module (quote evil-search))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
