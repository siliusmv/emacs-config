;;; =========================================================
;;; %%%%%%%%%%%%%%%% My Emacs init file %%%%%%%%%%%%%%%%%%%%%
;;; =========================================================

;; =========================================================
;; Startup optimisation
;; From https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
;; =========================================================

;; Set garbage collection threshold
;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

;; Set file-name-handler-alist
;; Also from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Set deferred timer to reset them
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
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


;; =========================================================
;; %%%%%%%%%%%%%%%%%%% BASIC SETTINGS %%%%%%%%%%%%%%%%%%%%%%
;; =========================================================
(use-package emacs
  :config
  ;; Remove menus and stuff
  (setq inhibit-startup-screen t) ;; Remove startup screen

  (setq column-number-mode t) ;; Display column numbers

  (global-hl-line-mode) ;; Highlight current line

  ;; Allow enclosing a marked region with $$
  (add-to-list 'insert-pair-alist (list ?\$ ?\$))

  ;; This must be done after loading GUI elements
  (defun remove-all-bars (frame)
    (select-frame frame)
    (if (display-graphic-p)
	(progn
	  (toggle-scroll-bar -1)
	  (menu-bar-mode -1)
	  (tool-bar-mode -1))))

  (if (daemonp)
      (add-hook 'after-make-frame-functions #'remove-all-bars)
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

  ;; Global keybindings
  (general-def
   "C-x k" 'kill-this-buffer
   )


  (general-define-key
   :states 'motion
   :keymaps 'override
   :prefix "]"
   "" nil
   ")" '(evil-next-close-paren :wk "next closing parenthesis")
   "}" '(evil-next-close-brace :wk "next closing brace")
   "]" '(evil-forward-section-begin :wk "next section")
   "s" '(flyspell-correct-next :wk "next spelling error")
   "e" '(flymake-goto-next-error :wk "flymake: next error")
   "f" '(flycheck-next-error :wk "flycheck: next error")
   "b" '(next-buffer :wk "next buffer")
   "w" '(persp-next :wk "next workspace")
   "t" '(centaur-tabs-forward :wk "next tab")
   )

  (general-define-key
   :states 'motion
   :keymaps 'override
   :prefix "["
   "" nil
   "(" '(evil-previous-open-paren :wk "prev opening parenthesis")
   "{" '(evil-previous-open-brace :wk "prev opening brace")
   "[" '(evil-backward-section-begin :wk "prev section")
   "s" '(flyspell-correct-previous :wk "prev spelling error")
   "e" '(flymake-goto-prev-error :wk "flymake: prev error")
   "f" '(flycheck-previous-error :wk "flycheck: prev error")
   "b" '(previous-buffer :wk "prev buffer")
   "w" '(persp-prev :wk "prev workspace")
   "t" '(centaur-tabs-backward :wk "prev tab")
   )

  ;; Space as leader
  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   ;; Popular keybindings
   "" nil
   "SPC" '(counsel-find-file :wk "find file")
   "," '(persp-switch-to-buffer :wk "switch buffer")
   "." '(save-buffer :wk "save buffer")
   "-" '(counsel-grep-or-swiper :wk "search in buffer")
   ;;"-" '(avy-goto-char-timer :wk "avy-timer")
   "g" '(magit-status :wk "git")
   "S" '(save-some-buffers :wk "save all buffers")

   ;; Buffer keymap
   "b" '(:ignore t :wk "buffers")
   "b k" '(kill-this-buffer :wk "kill buffer")
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
   "f r" '(ranger :wk "open ranger")
   "f d" '(dired :wk "open directory")
   "f SPC" '(counsel-find-file :wk "find files")
   "f t" '(treemacs :wk "file-tree")

   ;; Quit
   "q" '(:ignore t :wk "quit")
   "q f" '(delete-frame :wk "close frame")
   "q w" '(delete-window :wk "close window")
   "q o" '(delete-other-windows :wk "close other windows")
   "q W" '(kill-buffer-and-window :wk "Kill buffer, close window")
   "q F" '(siliusmv/kill-buffer-and-frame :wk "Kill buffer, close frame")

   ;; Search keymap
   "s" '(:ignore t :wk "search")
   "s b" '(counsel-grep-or-swiper :wk "search in buffer")
   "s 0" '(evil-ex-nohighlight :wk "turn off highlight")
   "s SPC" '(counsel-grep-or-swiper :wk "search in buffer")
   "s d" '(counsel-ag :wk "search in directory")
   "s g" '(counsel-git-grep :wk "search in git repository")
   ;; "s p" '(projectile-ripgrep :wk "search in project")

   ;; Project keymap
   "p" '(:keymap projectile-command-map :package projectile :wk "project menu")

   ;; Editing commands
   "e" '(:ignore t :wk "edit")
   "e (" '(insert-pair :wk "insert pair")
   "e [" '(insert-pair :wk "insert pair")
   "e {" '(insert-pair :wk "insert pair")
   "e \"" '(insert-pair :wk "insert pair")
   "e '" '(insert-pair :wk "insert pair")
   "e \$" '(insert-pair :wk "insert pair")
   "e `" '(insert-pair :wk "insert pair")
   "e )" '(delete-pair :wk "delete pair")
   "e d" '(siliusmv/cycle-dict :wk "cycle spell-check dictionary")

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
   ;; General: Toggle light/dark theme. Toggle flyspell and flycheck
   ;; Mode specific: (map to ",") toggle pdf reader in auctex. Cycle between R sessions ess. Toggle language for flyspell.

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
   "o m" '(mu4e :wk "email")
   "o c" '(siliusmv/open-calendar :wk "calendar")
   "o f" '(make-frame-command :wk "frame")
   "o T" '(treemacs :wk "file-tree")

   ;; "Workspaces"
   "TAB" '(:ignore t :wk "workspaces")
   "TAB n" '(persp-add-new :wk "new workspace")
   "TAB [" '(persp-next :wk "next workspace")
   "TAB ]" '(persp-prev :wk "prev workspace")
   "TAB b" '(persp-switch-to-buffer :wk "switch to buffer")
   "TAB k" '(persp-kill-buffer :wk "kill buffer")
   "TAB s" '(persp-frame-switch :wk "switch to workspace")
   "TAB S" '(persp-save-state-to-file :wk "save workspace conf.")
   "TAB l" '(persp-load-state-from-file :wk "load workspace conf.")
   "TAB x" '(persp-kill :wk "kill workspace")
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
 :states 'normal
 :keymaps 'dired-mode-map
 "l" 'dired-find-file
 "h" 'dired-up-directory
 "," '(:ignore t)
 ", ." '(dired-hide-dotfiles :wk "hide dotfiles"))





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
  :init
  ;; (global-nlinum-relative-mode)
  :config
  (setq nlinum-highlight-current-line t)
  ;(nlinum-relative-setup-evil)             ;; setup for evil
  (setq nlinum-relative-redisplay-delay 0)      ;; delay
  (setq nlinum-relative-current-symbol "")
  (add-hook 'text-mode-hook 'nlinum-relative-mode)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)

  (defun siliusmv/nlinum-off ()
    (interactive)
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
(use-package mu4e
  :after evil
  :bind ("C-c m" . mu4e)
  :general
  (:keymaps '(mu4e-headers-mode-map mu4e-view-mode-map mu4e-main-mode-map)
   :states '(motion emacs)
   :prefix ","
   :non-normal-prefix "C-,"
   "g" '(siliusmv/gmail-firefox :wk "gmail")
   "o" '(siliusmv/outlook-firefox :wk "outlook")
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



;; =========================================================
;; Text navigation
;; =========================================================

(use-package avy

  :general
  (:states '(normal visual)
   "s" 'avy-goto-char-timer
   )
  :config
  (setq avy-style 'words)
  )

(use-package ace-window

  :defer t
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
   :states '(motion insert)
   :keymaps 'company-active-map
   "C-j" 'company-complete-selection
   "<return>" '(:ignore t)
   "<tab>" 'company-complete-common
   "C-n" 'company-select-next
   "C-p" 'company-select-previous
   "C-l" 'counsel-company
   )

  (general-define-key
   :states '(motion insert)
   :keymaps 'company-mode-map
   "C-<tab>" 'company-other-backend
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
  (:keymaps 'ess-r-mode-map
   :prefix ","
   :non-normal-prefix "C-,"
   :states '(motion emacs)
   "r" '(run-ess-r :wk "Open new R session")
   )
  :diminish
  ((ess-r-package-mode . "")
   (eldoc-mode . ""))
  :init
  (require 'ess-r-mode)
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
	 '(company-capf ; Works together with eglot
	   company-files
	   (company-R-args
	    company-R-objects
 	    company-R-library :separate)
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
	doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  ;; (load-theme 'doom-one t)

  (defvar siliusmv/my-themes
    (list
     '("dark" doom-one)
     '("light" doom-one-light)))

  (defvar siliusmv/initial-theme "dark"
    "Initial theme for starting up emacs")

  (defun siliusmv/choose-theme (&optional theme-short)
    (interactive)
    (if (not theme-short)
	(setq theme-short
	      (ivy-read "Select theme: " siliusmv/my-themes)))
    (let ((theme-name (nth 1 (assoc theme-short siliusmv/my-themes))))
      (load-theme theme-name t)))

  (siliusmv/choose-theme siliusmv/initial-theme)
  )

;; =========================================================
;; Wokspaces
;; =========================================================
(use-package persp-mode

  :init
  (persp-mode 1)
  :config
  ;; This was advised from https://github.com/Bad-ptr/persp-mode.el
  (setq wg-morph-on nil)
  (setq persp-autokill-buffer-on-remove 'kill-weak)

  (setq persp-nil-hidden t ;; Hide nil-perspecive
	persp-auto-save-opt (if noninteractive 0 1)
	persp-nil-name "main"
	persp-auto-save-fname "autosave"
	persp-auto-resume-time -1 ;; Do not autoresume
	persp-remove-buffers-from-nil-persp-behaviour nil
	persp-init-frame-behaviour nil ;; Open scratch for new frames
	)
  )

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
  :bind ("C-x g" . 'magit-status)
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
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
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
  :init
  ;; The default search is ivy--regex-plus
  (setq ivy-re-builders-alist
	'((t . ivy--regex-ignore-order))) ;; Regexps can interchange order

  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t) ;; Proposed from the Ivy wiki
  (setq ivy-count-format "(%d/%d) ") ;; Proposed from the Ivy wiki
  ; (setq ivy-extra-directories nil) ;; Remove ./ and ../ from searches

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
(use-package flycheck

  ;;:hook (prog-mode . flycheck-mode)
  ;;:diminish flycheck-mode
  )


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
   :states '(normal insert emacs)
   :prefix ","
   :non-normal-prefix "C-,"
   "" nil
   "v" '(TeX-view :wk "view pdf")
   "c" '(TeX-command-master :wk "compile document")
   "," '(reftex-toc :wk "navigate document")
   "r" '(reftex-toc-Rescan :wk "refresh reftex")
   "e" '(TeX-next-error :wk "compilation errors")
   "f" '(LaTeX-fill-buffer :wk "fill buffer")
   "t" '(:ignore t :wk "toggle")
   "t f" '(siliusmv/toggle-tex-fold :wk "folding")
   "t v" '(siliusmv/toggle-latex-pdf-viewer :wk "PDF viewer")
  )

  :init

  (add-hook 'LaTeX-mode-hook 'latex-math-mode)


  (setq 
   TeX-auto-global "~/.emacs.d/auctex/auto-global"
   TeX-auto-regexp-list 'TeX-auto-full-regexp-list)

  ;;; Functions for changing PDF viewers

  (defun siliusmv/choose-latex-pdf-viewer (viewer)
    "Change PDF viewer for latex"
    (cond
     ((equal viewer "evince")
      (delete '(output-pdf "Evince") TeX-view-program-selection)
      (setq TeX-view-program-selection
  	    (cons '(output-pdf "Evince") TeX-view-program-selection)))
     ((equal viewer "pdf-tools")
      (progn
  	(unless (assoc "PDF Tools" TeX-view-program-list-builtin)
  	  (add-to-list 'TeX-view-program-list-builtin
  		       '("PDF Tools" TeX-pdf-tools-sync-view)))
  	(delete '(output-pdf "PDF Tools") TeX-view-program-selection)
  	(setq TeX-view-program-selection
  	      (cons '(output-pdf "PDF Tools") TeX-view-program-selection))))
     ((equal viewer "zathura")
      (progn
  	;; Add backwards search to zathura
  	;; https://www.emacswiki.org/emacs/AUCTeX#toc23
  	(add-to-list 'TeX-view-program-list
  		     '("Zathura2"
  		       ("zathura %o"
  			(mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=my-gui-server --no-wait +%{line} %{input}\""))
  		       "zathura"))
  	(delete '(output-pdf "Zathura2") TeX-view-program-selection)
  	(setq TeX-view-program-selection
  	      (cons '(output-pdf "Zathura2") TeX-view-program-selection))))))

  (defvar siliusmv/latex-pdf-viewer "evince"
    "Variable used in siliusmv/toggle-latex-pdf-viewer")

  (defun siliusmv/toggle-latex-pdf-viewer ()
    "Toggle PDF viewer for latex"
    (interactive)
    (cond
     ((equal siliusmv/latex-pdf-viewer "evince")
      (progn
  	(setq siliusmv/latex-pdf-viewer "zathura")
  	(siliusmv/choose-latex-pdf-viewer siliusmv/latex-pdf-viewer)))
     ((equal siliusmv/latex-pdf-viewer "zathura")
      (progn
  	(setq siliusmv/latex-pdf-viewer "pdf-tools")
  	(siliusmv/choose-latex-pdf-viewer siliusmv/latex-pdf-viewer)))
     ((equal siliusmv/latex-pdf-viewer "pdf-tools")
      (progn
  	(setq siliusmv/latex-pdf-viewer "evince")
  	(siliusmv/choose-latex-pdf-viewer siliusmv/latex-pdf-viewer))))
    (message (concat "Switch PDF viewer to " siliusmv/latex-pdf-viewer)))


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

  :config

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

  (TeX-add-style-hook
   "report"
   (lambda ()
     (TeX-add-to-alist 'LaTeX-provided-package-options
		       '(("inputenc" "utf8") ("biblatex" "bibstyle=apa" "citestyle=authoryear" "natbib=true" "backend=biber" "maxcitenames=3" "mincitenames=1" "maxbibnames=99" "firstinits=true") ("tocbibind" "nottoc") ("caption" "margin=1cm" "labelfont=bf")))
     (TeX-run-style-hooks
      "inputenc"
      "biblatex"
      "hyperref"
      "graphicx"
      "amsmath"
      "amssymb"
      "todonotes"
      "enumitem"
      "setspace"
      "fancyhdr"
      "tocbibind"
      "caption"
      "textcomp"
      "bm"
      "gensymb"
      "upgreek"
      "chngcntr"
      "placeins"
      "algorithm"
      "algpseudocode"
      "amsthm"
      "tikz")
     (TeX-add-symbols
      '("norm" 1)
      '("partiald" 2)
      '("derivative" 2)
      "dd"
      "i"
      "j"))
   :latex)






  
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

  ;; Not sure that this works
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)
  (setq ispell-dictionary "en_GB")

  
  (defvar siliusmv/my-dictionaries
    (list
     '("british" "en_GB")
     '("norwegian" "no_BOK")))

  (defun siliusmv/choose-dictionary (&optional dict)
    (interactive)
    (if (not dict)
	(setq dict
	      (ivy-read "Select dictionary: " siliusmv/my-dictionaries)))
    (let ((dict-name (nth 1 (assoc dict siliusmv/my-dictionaries))))
      (ispell-change-dictionary dict-name)))


  (use-package flyspell-correct-ivy

    :demand t
    :bind (:map flyspell-mode-map
		("C-c $" . flyspell-correct-word-generic)))
  ;; Map this key with the prefix "," as well
  )


;; =========================================================
;; PDF Tools
;; =========================================================

(use-package pdf-tools

  ;;:pin manual ;; manually update
  :config
  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (evil-collection-define-key 'normal 'pdf-view-mode-map
    "f" 'pdf-links-action-perform
    "J" 'pdf-view-next-page-command
    "K" 'pdf-view-previous-page-command
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
;; Calendar
;; =========================================================
(use-package calfw

  :defer 5
  :commands (cfw:open-calendar-buffer siliusmv/open-calendar)
  :general
  (:keymaps '(cfw:calendar-mode-map cfw:details-mode-map)
   :states '(motion emacs)
   :prefix ","
   :non-normal-prefix "C-,"
   "g" '(siliusmv/gcal-firefox :wk "gmail")
   "o" '(siliusmv/outlook-cal-firefox :wk "outlook")
   )

  :init

  (defun siliusmv/gcal-firefox ()
    "Open google calendar in firefox"
    (interactive)
    (browse-url-firefox "https://calendar.google.com/calendar/r" t)
    )

  (defun siliusmv/outlook-cal-firefox ()
    "Open outlook calendar in firefox"
    (interactive)
    (browse-url-firefox "https://mail.ntnu.no/owa/silius.m.vandeskog@ntnu.no/#path=/calendar/view/Month" t)
    )

  :config

  (use-package calfw-ical

    )

  (defun siliusmv/open-calendar ()
    "Collect different calendars and display them"
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      ;;(cfw:org-create-source "Green")  ; orgmode source
      ;;(cfw:howm-create-source "Blue")  ; howm source
      ;;(cfw:cal-create-source "Orange") ; diary source
      ;;(cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
      (cfw:ical-create-source
       "gcal"
       "https://calendar.google.com/calendar/ical/siliusv%40gmail.com/private-bd7c25c2f3af07ae6b2c126f76822e3b/basic.ics"
       "Blue") ; google calendar ICS
      (cfw:ical-create-source
       "work"
       "https://outlook.office365.com/owa/calendar/8973f4f445fd477282c6ae68a234b61a@stud.ntnu.no/5a904a8004824dd2a0afad309bceb32b1928256821864056809/calendar.ics"
       "Red") ; Work calendar
      ))) 
  
  (setq cfw:face-item-separator-color nil
        cfw:render-line-breaker 'cfw:render-line-breaker-none
        cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓)
  )

;; =========================================================
;; Project management
;; =========================================================
(use-package projectile

  :diminish projectile-mode
  :bind ("C-c p" . 'projectile-command-map)
  :defer t
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  )

;; Install ag or ripgrep!!!!


;; =========================================================
;; Terminal
;; =========================================================
(use-package multi-term
  :bind ("C-c t" . multi-term)
  :commands (multi-term)

  :general
  (:keymaps 'term-mode-map
   :states '(normal insert emacs)
   :prefix ","
   :non-normal-prefix "C-,"
   "" nil

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
;; File navigation
;; =========================================================
(use-package ranger
  :bind
  (("C-c f" . ranger))
  :defer t
  :config
  (setq ranger-cleanup-eagerly t) ;; Cleanup opened preview buffers
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
;; Other stuff
;; =========================================================

(require 'iso-transl)


(setenv "GPG_AGENT_INFO" nil)
(setq epg-gpg-program "gpg2")
 
(use-package treemacs
  :after evil
  :commands (treemacs)
  :config
  (fringe-mode '(1 . 1))
  (setq treemacs-fringe-indicator-mode nil
	treemacs-no-png-images t
	treemacs-width 40
	treemacs-silent-refresh t
	treemacs-silent-filewatch t
	treemacs-file-event-delay 1000
	treemacs-file-follow-delay 0.1))

  (use-package treemacs-evil
    :after treemacs
    :config
    (evil-define-key 'treemacs treemacs-mode-map (kbd "l") 'treemacs-RET-action)
    (evil-define-key 'treemacs treemacs-mode-map (kbd "h") 'treemacs-TAB-action))

(use-package treemacs-projectile)

(use-package all-the-icons :config (setq all-the-icons-scale-factor 1.0))

(use-package page-break-lines)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((projects  . 5)
			  ;;(bookmarks . 5)
			  (recents . 5)
			  ;;(agenda . 5)
			  ;;(registers . 5)
			  ))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-banner-logo-title "Welcome back!")
  )

;; (use-package company-quickhelp
;;   :hook (global-company-mode . company-quickhelp-mode)
;;   :init (setq company-quickhelp-delay 0.5)
;;   )

;; (use-package company-box
;;   :hook (company-mode . company-box-mode)
;; 
;;   :config
;; 
;;   ;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-company.el#L76
;;   (when (and (display-graphic-p)
;; 	     (require 'all-the-icons nil t))
;;     (declare-function all-the-icons-faicon 'all-the-icons)
;;     (declare-function all-the-icons-material 'all-the-icons)
;;     (setq company-box-icons-all-the-icons
;; 	  `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
;; 	    (Text . ,(all-the-icons-faicon "text-width" :height 0.85 :v-adjust -0.05))
;; 	    (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
;; 	    (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
;; 	    (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
;; 	    (Field . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
;; 	    (Variable . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
;; 	    (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
;; 	    (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
;; 	    (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
;; 	    (Property . ,(all-the-icons-faicon "wrench" :height 0.85 :v-adjust -0.05))
;; 	    (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
;; 	    (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
;; 	    (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
;; 	    (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
;; 	    (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))
;; 	    (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
;; 	    (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
;; 	    (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
;; 	    (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05))
;; 	    (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
;; 	    (Constant . ,(all-the-icons-faicon "square-o" :height 0.9 :v-adjust -0.05))
;; 	    (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
;; 	    (Event . ,(all-the-icons-faicon "bolt" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-orange))
;; 	    (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
;; 	    (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
;; 	    (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2)))
;; 	  company-box-icons-alist 'company-box-icons-all-the-icons))
;; 
;;   )


;; (use-package solaire-mode
;;   :hook
;;   ( ;((change-major-mode after-revert ediff-prepare-buffer) . 'turn-on-solaire-mode)
;;    (minibuffer-setup . solaire-mode-in-minibuffer))
;; ;;   :config
;; ;; 
;; ;;   ;; solaire-mode messes with hl-line-mode when using the daemon
;; ;;   (defun siliusmv/add-solaire (frame)
;; ;;     (select-frame frame)
;; ;;     (if (display-graphic-p)
;; ;; 	(if (not (bound-and-true-p solaire-mode))
;; ;; 	    (progn
;; ;; 	      (solaire-global-mode)
;; ;; 	      (solaire-mode-swap-bg) ; Only necessary for doom-themes
;; ;; 	      ))
;; ;;       ;; This part is for when you open a non-graphical frame and everything
;; ;;       ;; gets really ugly
;; ;;       (if siliusmv/is-dark-theme
;; ;; 	  (load-theme 'doom-one t)
;; ;; 	(load-theme 'doom-one-light t))))
;; ;; 
;; ;;   (if (daemonp)
;; ;;       (add-hook 'after-make-frame-functions #'siliusmv/add-solaire)
;; ;;     (solaire-global-mode)
;; ;;     (solaire-mode-swap-bg))
;; 
;;   )


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
