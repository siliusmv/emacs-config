;;; My Emacs init file

;;;; TODO
;; Ensure that the dictionary in auctex is correct, and not "default"
;; Figure out how to control the kill-ring
;; Add expand-region
;; Speed up eglot in some way?

;;; Non-package specific stuff
;;;; Global variables and constants
(defvar s/init-theme "light") ; Default theme
(defvar s/init-dict "british") ; Default language
(defvar s/gc-cons-threshold (* 1024 1024 50)) ; Threshold for garbage disposal
(defvar s/macos-p (string-equal system-type "darwin")) ; Is this a mac?
(defvar s/literature-dir "~/OneDrive - NTNU/literature/")
(setq-default fill-column 100) ; Column for starting automatic line wrap
(setq-default default-input-method "TeX") ; Input method activated by the command toggle-input-method

;;;; Startup optimisation

;; Set garbage collection threshold
;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
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
	 '("LANG" "TEXINPUTS" "BIBINPUTS" "LC_ALL" "LANG" "R_PROFILE_USER"))
  (exec-path-from-shell-initialize))


;;;; BASIC SETTINGS

(setq inhibit-startup-screen t) ; Remove startup screen
(setq column-number-mode t) ; Display column numbers
(global-hl-line-mode) ; Highlight current line
(setq ring-bell-function 'ignore) ; Stop the error bell sound
(fset 'yes-or-no-p 'y-or-n-p) ; Change all prompts to y or n

(scroll-bar-mode -1) ; Remove scroll bar
(tool-bar-mode -1) ; Remove tool bar
(if (not s/macos-p) (menu-bar-mode -1)) ; Sometimes remove menu bar

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

;;; Package specific settings
;;;; Evil-mode

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom (evil-collection-minibuffer-setup t)
  :config
  (evil-collection-init))

;;;; Diminish
;; Don't let active modes clutter the mode-line
(use-package diminish)

;;;; Smooth scrolling
;; Keep the pointer in the center of the screen
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

;;;; General.el - keybindings
;; Keybindings
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer s/leader-def
    :prefix "SPC"
    :global-prefix "M-SPC"
    :states '(normal visual motion insert emacs)
    :keymaps 'override)

  (general-create-definer s/local-leader-def
    :prefix "SPC m"
    :global-prefix "M-SPC m"
    :states '(normal visual motion insert emacs))

  (general-create-definer s/goto-leader-def
					;:global-prefix "M-g"
    :prefix "M-g"
    :states '(normal visual motion insert emacs)
					;:keymaps '(prog-mode-map text-mode-map dired-mode-map TeX-mode-map))
    :keymaps 'override)

  (general-create-definer s/insert-unicode
    :prefix "`"
    ;:states '(insert emacs)
    :keymaps '(prog-mode-map text-mode-map ess-mode-map inferior-ess-mode-map evil-ex-completion-map minibuffer-local-map swiper-map))

  (general-define-key
   :keymaps 'override
   :states '(normal visual insert)
   "M-?" '(which-key-show-top-level :wk "show all bindings")
   "M-o" '(ace-window :wk "other window")
   "M-s" '(save-buffer :wk "save buffer")
   "M-S" '(save-some-buffers :wk "save all buffers")
   "M-`" '(ns-next-frame :wk "switch frame"))

  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "/" '(counsel-grep-or-swiper :wk "search in buffer")
   "M-k" '(scroll-down-command :wk t)
   "M-j" '(scroll-up-command :wk t)
   "M-h" '(evil-jump-backward :wk "jump backward")
   "M-l" '(evil-jump-forward :wk "jump forward"))

  (general-define-key
   :keymaps 'minibuffer-local-map
   "M-p" '(yank :wk "copy from clipboard")) 

  (general-define-key
   :states 'visual
   :keymaps 'override
   "<tab>" 'indent-for-tab-command) ;; Indent a large area
  
  ;; Allow enclosing a marked region with $$
  (add-to-list 'insert-pair-alist (list ?\$ ?\$))
  ;; Editing commands
  (general-define-key
   :states 'visual
   :keymaps 'override
   :prefix "M-i"
   "" '(:ignore t :wk "insert")
   "(" '(insert-pair :wk "(")
   "[" '(insert-pair :wk "[")
   "{" '(insert-pair :wk "{")
   "\"" '(insert-pair :wk "\"")
   "\'" '(insert-pair :wk "\'")
   "\$" '(insert-pair :wk "\$")
   "`" '(insert-pair :wk "`")
   ")" '(delete-pair :wk "delete pair"))

  (s/goto-leader-def
    "" '(:ignore t :wk "go to...")
    "l" '(avy-goto-line :wk "line")
    "e" '(flymake-goto-next-error :wk "next error (flymake)")
    "E" '(flymake-goto-prev-error :wk "prev error (flymake)")
    "b" '(evil-next-buffer :wk "next buffer")
    "B" '(evil-prev-buffer :wk "prev buffer")
    "s" '(flyspell-correct-next :wk "next spelling error")
    "S" '(flyspell-correct-previous :wk "prev spelling error")
    "t" '(persp-next :wk "next tab")
    "T" '(persp-prev :wk "prev tab")
    "r" '(revert-buffer :wk "refresh buffer")

    "d" '(:ignore t :wk "definition")
    "d i" '(ivy-imenu-anywhere :wk "with imenu")
    "d d" '(dumb-jump-go :wk "with dumb-jump")
    "d x" '(xref-find-definitions :wk "with xref"))

  (s/insert-unicode
    "" '(:ignore t :wk "insert unicode")
    "`" (lambda () (interactive) (insert "`") :wk "`")

    "q" (lambda () (interactive) (insert "χ") :wk "χ")
    "w" (lambda () (interactive) (insert "ω") :wk "ω")
    "e" (lambda () (interactive) (insert "ε") :wk "ε")
    "r" (lambda () (interactive) (insert "ρ") :wk "ρ")
    "t" (lambda () (interactive) (insert "τ") :wk "τ")
    "y" (lambda () (interactive) (insert "ψ") :wk "ψ")
    "u" (lambda () (interactive) (insert "υ") :wk "υ")
    "p" (lambda () (interactive) (insert "π") :wk "π")
    "a" (lambda () (interactive) (insert "α") :wk "α")
    "s" (lambda () (interactive) (insert "σ") :wk "σ")
    "d" (lambda () (interactive) (insert "δ") :wk "δ")
    "f" (lambda () (interactive) (insert "ϕ") :wk "ϕ")
    "g" (lambda () (interactive) (insert "γ") :wk "γ")
    "h" (lambda () (interactive) (insert "η") :wk "η")
    "j" (lambda () (interactive) (insert "θ") :wk "θ")
    "k" (lambda () (interactive) (insert "κ") :wk "κ")
    "l" (lambda () (interactive) (insert "λ") :wk "λ")
    "z" (lambda () (interactive) (insert "ζ") :wk "ζ")
    "x" (lambda () (interactive) (insert "ξ") :wk "ξ")
    "b" (lambda () (interactive) (insert "β") :wk "β")
    "n" (lambda () (interactive) (insert "ν") :wk "ν")
    "m" (lambda () (interactive) (insert "μ") :wk "μ")
    "," (lambda () (interactive) (insert "ℓ") :wk "ℓ")

    "Q" (lambda () (interactive) (insert "Χ") :wk "Χ")
    "W" (lambda () (interactive) (insert "Ω") :wk "Ω")
    "E" (lambda () (interactive) (insert "Ε") :wk "Ε")
    "R" (lambda () (interactive) (insert "Ρ") :wk "Ρ")
    "T" (lambda () (interactive) (insert "Τ") :wk "Τ")
    "Y" (lambda () (interactive) (insert "Ψ") :wk "Ψ")
    "U" (lambda () (interactive) (insert "Υ") :wk "Υ")
    "P" (lambda () (interactive) (insert "Π") :wk "Π")
    "A" (lambda () (interactive) (insert "Α") :wk "Α")
    "S" (lambda () (interactive) (insert "Σ") :wk "Σ")
    "D" (lambda () (interactive) (insert "Δ") :wk "Δ")
    "F" (lambda () (interactive) (insert "ϕ") :wk "ϕ")
    "G" (lambda () (interactive) (insert "Γ") :wk "Γ")
    "H" (lambda () (interactive) (insert "Η") :wk "Η")
    "J" (lambda () (interactive) (insert "Θ") :wk "Θ")
    "K" (lambda () (interactive) (insert "Κ") :wk "Κ")
    "L" (lambda () (interactive) (insert "Λ") :wk "Λ")
    "Z" (lambda () (interactive) (insert "Ζ") :wk "Ζ")
    "X" (lambda () (interactive) (insert "Ξ") :wk "Ξ")
    "B" (lambda () (interactive) (insert "Β") :wk "Β")
    "N" (lambda () (interactive) (insert "Ν") :wk "Ν")
    "M" (lambda () (interactive) (insert "Μ") :wk "Μ")
    )

  (s/leader-def
    "" nil
    "M-SPC" '(counsel-find-file :wk "find file")
    "SPC" '(counsel-find-file :wk "find file")
    "~" '(s/go-to-config :wk "go home")

    "g" (general-simulate-key "M-g" :which-key "go to...")
    "m" '(:ignore t :wk "mode specific")

    "c" '(counsel-compile :wk "compile")

    ;; Yasnippet keymap
    "y" '(:ignore t :wk "yasnippet")
    "y i" '(yas-insert-snippet :wk "insert")
    "y c" '(yas-new-snippet :wk "create")
    "y r" '(yas-reload-all :wk "reload")
    
    ;; Buffer keymap
    "b" '(:ignore t :wk "buffers")
    "b k" '(s/kill-this-buffer :wk "kill buffer")
    "b K" '(kill-buffer :wk "kill some buffer")
    "b s" '(save-buffer :wk "save buffer")
    "b r" '(rename-buffer :wk "rename buffer")
    "b b" '(counsel-switch-buffer :wk "switch buffer")

    ;; Help keymap
    "h" '(:ignore t :wk "help")
    "h k" '(describe-key :wk "keys")
    "h v" '(describe-variable :wk "variables")
    "h ?" '(help-for-help :wk "all the help-possibilities")
    "h i" '(info :wk "read the manual")
    "h m" '(describe-mode :wk "modes")
    "h f" '(describe-function :wk "functions")

    ;; Frame manipulation
    "F" '(:ignore t :wk "frame manipulation")
    "F |" '(tile-frames-horizontally :wk "tile horisontally")
    "F -" '(tile-frames-vertically :wk "tile vertically")
    "F F" '(toggle-max-frame :wk "toggle full-screen")
    "F K" '(toggle-max-frame-vertically :wk "toggle vertical full-screen")
    "F H" '(toggle-max-frame-horizontally :wk "toggle horisontal full-screen")
    "F l" '(move-frame-to-screen-right :wk "move to right side")
    "F h" '(move-frame-to-screen-left :wk "move to left side")
    "F k" '(move-frame-to-screen-up :wk "move to top")
    "F j" '(move-frame-to-screen-down :wk "move to bottom")
    
    ;; Quit
    "q" '(:ignore t :wk "quit")
    "q f" '(delete-frame :wk "close frame")
    "q w" '(delete-window :wk "close window")
    "q o" '(delete-other-windows :wk "close other windows")
    "q W" '(kill-buffer-and-window :wk "Kill buffer, close window")
    "q F" '(s/kill-buffer-and-frame :wk "Kill buffer, close frame")

    ;; Search keymap
    "s" '(:ignore t :wk "search")
    "s b" '(counsel-grep-or-swiper :wk "search in buffer")
    "s 0" '(evil-ex-nohighlight :wk "turn off highlight")
    "s d" '(counsel-ag :wk "search in directory")
    "s g" '(counsel-git-grep :wk "search in git repository")
    ;; "s p" '(projectile-ripgrep :wk "search in project")

    ;; Project keymap
    "p" '(:keymap projectile-command-map :package projectile :wk "project menu")

    ;; Variables keymap
    "v" '(:ignore t :wk "change variables")
    "v d" '(s/choose-dictionary :wk "spell-check dictionary")
    "v t" '(s/choose-theme :wk "theme")
    "v s" '(flyspell-mode :wk "toggle spelling")
    "v F" '(flycheck-mode :wk "toggle flycheck")
    "v f" '(:ignore t :wk "font size")
    "v f +" '(default-text-scale-increase :wk "enlarge")
    "v f -" '(default-text-scale-decrease :wk "decrease")
    "v i" '(toggle-input-method :wk "input method (TeX)")

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
    "w d" '(delete-window :wk "delete window")
    "w f" '(delete-other-windows :wk "focus on window")
    "w v" '(split-window-right :wk "split vertical")
    "w s" '(split-window-below :wk "split")
    "w o" '(ace-window :wk "other window")

    ;; "Open programs" - keymap
    "o" '(:ignore t :wk "open ...")
    "o d" '(dired :wk "dired")
    "o t" '(vterm-other-window :wk "terminal")
    "o g" '(magit-status :wk "git")
    "o e" '(eshell :wk "eshell")
    "o r" '(run-ess-r :wk "R")

    ;; "Workspaces (tabs)"
    "t" '(:ignore t :wk "workspaces")
    "t o" '(persp-switch :wk "other workspace")
    "t n" '(persp-next :wk "next")
    "t p" '(persp-prev :wk "prev")
    "t r" '(persp-rename :wk "rename")
    "t d" '(persp-kill :wk "close")
    ))


;;;; Change text size
;; This contains the functions default-text-scale-(increase/decrease)
(use-package default-text-scale)

;;;; Dired stuff

(defun s/dired-hide-dotfiles ()
  "Hides all dotfiles in a dired-buffer"
  (dired-mark-files-regexp "^\\.")
  (dired-do-kill-lines))

;; This is necessary for letting us use SPC in dired
(defun s/dired-keybinds ()
  (general-def
     :keymaps 'dired-mode-map
     :states '(normal visual insert motion emacs)
     "SPC" nil
     "SPC m" nil)
   (s/local-leader-def
     :keymaps 'dired-mode-map
     "h" '(s/dired-hide-dotfiles :wk "hide dotfiles")
     "c" '(dired-do-copy :wk "copy")
     "m" '(dired-do-rename :wk "move")
     "d" '(dired-do-delete :wk "delete")
     "s" '(dired-do-symlink :wk "symlink")
     "+" '(dired-create-directory :wk "mkdir")
     "R" '(dired-do-rename-regexp :wk "rename regexp"))
   (general-define-key
    :keymaps 'dired-mode-map
    :states 'normal
    "l" 'dired-find-file
    "h" 'dired-up-directory))
(run-with-idle-timer 2 nil 's/dired-keybinds)

;; Set dired ls arguments
(setq dired-listing-switches "-Alh")

;; Smart guessing of target directory for copying etc.
(setq dired-dwim-target t)

;;;; Elisp stuff
(s/local-leader-def
 :keymaps 'emacs-lisp-mode-map
 "o" '(eval-defun :wk "evaluate outer sexp")
 "i" '(eval-last-sexp :wk "evaluate inner sexp")
 )


;;;; Language servers
(use-package eglot
  :config
  (setq
   eglot-stay-out-of '(company) ; Don't mess with company backends
   ;; Avoid annoying highlighting of everything
   eglot-ignored-server-capabilites
   '(:hoverProvider :definitionProvider :typeDefinitionProvider :implementationProvider :implementationProvider
		    :referencesProvider :documentHighlightProvider :documentSymbolProvider :workspaceSymbolProvider
		    :codeLensProvider :documentFormattingProvider :documentRangeFormattingProvider :documentLinkProvider)
   ))

(use-package flymake
  :init
  ;; Change FlyMake to FM on the mode-line
  (defun flymake--transform-mode-line-format (ret)
    "Change the output of `flymake--mode-line-format'."
    (setf (seq-elt (car ret) 1) " FM")
    ret)
  (advice-add #'flymake--mode-line-format
	      :filter-return #'flymake--transform-mode-line-format))

;;;; Text navigation

(use-package avy
  :general
  (:states '(normal visual)
   "s" 'avy-goto-char-timer
   )
  :config
  (setq avy-style 'at-full
	avy-all-windows t
	avy-timeout-seconds 0.5))

(use-package ace-window
  :config
  (setq
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) ; letters for switching window
   aw-scope 'frame ;; Only switch windows in the focused frame
   aw-background nil  ; Don't remove colour around the letters
   ))

;;;; Company
(use-package company
  :delight
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (general-define-key
   :keymaps 'company-active-map
   "<tab>" nil
   "TAB" nil
   "<backtab>" nil
   "S-TAB" nil
   "<return>" nil
   "M-l" 'company-complete
   "M-j" 'company-select-next
   "M-k" 'company-select-previous
   "M-n" 'company-other-backend
					;"M-/" 'company-search-candidates
   "M-/" 'counsel-company
   "M-S" '(counsel-company :wk "counsel-company"))

  (general-define-key
   :keymaps 'company-search-map
   "M-j" 'company-search-repeat-forward
   "M-k" 'company-search-repeat-backward)

  (general-define-key
   :states 'insert
   :keymaps 'company-mode-map
   "M-n" 'company-other-backend)

  ;; set default `company-backends'
  (setq company-backends
	'(company-capf
	  company-files ; files & directory
					;(company-abbrev company-dabbrev :separate)
	  ))

  ;; Behavoiur of completion pop-up
  (setq company-selection-wrap-around t ; Start at the top after reaching the bottom
	company-tooltip-align-annotations t
	company-idle-delay .1 ; Waiting time before we start completion
	company-minimum-prefix-length 1 ; Minimum letters before we start completion
	company-tooltip-limit 10)

  ;; Settings for backends
  (setq company-dabbrev-downcase nil
	company-dabbrev-code-ignore-case t
	company-dabbrev-ignore-case t
	company-dabbrev-code-other-buffers nil) ; Search other buffers with same major mode

  )

(use-package company-statistics
  :config
  (add-hook 'after-init-hook 'company-statistics-mode))


;;;; ESS (Emacs Speaks Statistics)
(use-package ess
  :init
  (require 'ess-r-mode)
  (defun s/ess-r-company ()
    "Set company backends for R buffers"
    (set (make-local-variable 'company-backends)
	 '(company-capf
	   company-files
	   (company-R-args company-R-objects company-R-library :separate))))

  :general
  (s/local-leader-def
    :keymaps '(ess-r-mode-map inferior-ess-mode-map)
    "r" '(run-ess-r :wk "Open new R session")
    "s" '(ess-switch-process :wk "Switch ESS session")
    "t" '(ess-r-devtools-test-package :wk "test package")
    "l" '(ess-r-devtools-load-package :wk "load package")
    "b" '(ess-r-devtools-build-package :wk "build package")
    "i" '(ess-r-devtools-install-package :wk "install package")
    "u" '(ess-r-devtools-unload-package :wk "unload package")
    )
  (:keymaps '(ess-r-mode-map inferior-ess-mode-map)
   :states '(motion normal insert visual emacs)
   "M-e" 'ess-eval-region-or-line-visibly-and-step
   "M-RET" 'ess-eval-region-or-function-or-paragraph-and-step
   )
  :diminish
  ((ess-r-package-mode . "")
   (eldoc-mode . ""))
  :config

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

  (setq ess-use-company nil) ; Don't let ESS decide backends
  
  ;; Company stuff
  (add-hook 'ess-r-mode-hook 's/ess-r-company)
  (add-hook 'inferior-ess-r-mode-hook (lambda () (company-mode -1)))

  (add-hook 'ess-mode-hook 'eglot-ensure)
  (add-hook 'inferior-ess-mode-hook '(lambda () (electric-pair-local-mode -1)))
)

;;;; Julia

(use-package julia-mode
  :config
  (add-hook 'julia-mode-hook 'julia-snail-mode)
  (add-hook 'julia-mode-hook 'yas-minor-mode))

;; You have to go into the source code of the function
;; eglot-jl--ls-invocation and comment out the line where
;; they change the environment variable JULIA_LOAD_PATH
;; for this to not destroy everything
(use-package eglot-jl
  :init
  (setq eglot-jl-default-environment "~/.julia/environments/v1.4")
  (eglot-jl-init)
  :config
  (add-hook 'julia-mode-hook 'eglot-ensure))

; (use-package julia-repl
;   :general
;   (:keymaps 'julia-repl-mode-map
;    "M-e" 'julia-repl-send-line
;    "M-RET" 'julia-repl-send-region-or-line)
; )

(use-package julia-snail
  :init
  (defun s/julia-snail-send-line-and-step ()
    (interactive)
    (julia-snail-send-line)
    (evil-next-line))
  :general
  (:keymaps 'julia-snail-mode-map
   "M-e" 's/julia-snail-send-line-and-step
   "M-RET" 'julia-snail-send-top-level-form)
  (s/local-leader-def
    :keymaps 'julia-mode-map
    "j" '(julia-snail :wk "julia"))
  )

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
;;;; Wokspaces

(use-package perspective
  :init (persp-mode))

(use-package persp-projectile
  :after perspective
  :general
  (:keymaps 'projectile-command-map
   "p" '(projectile-persp-switch-project :wk "switch project"))
  (s/leader-def
    "b b" '(persp-counsel-switch-buffer :wk "switch buffer")
    "b B" '(counsel-switch-buffer :wk "switch buffer globally"))
  )

;;;; Resize frames
(use-package frame-cmds)


;;;; Undo-tree
(use-package undo-tree
  :diminish
  :config
  ;; Always have it on
  (global-undo-tree-mode)
  (setq
   ;; Each node in the undo tree should have a timestamp
   undo-tree-visualizer-timestamps t
   ;; Show a diff window displaying changes
   undo-tree-visualizer-diff t))


;;;; Magit
(use-package magit
  :commands (magit-status)
  :config
  ;; Possible performance fix
  (setq auto-revert-buffer-list-filter
	'magit-auto-revert-repository-buffer-p)

  (use-package evil-magit) ;; Evil-movements in magit
  )


;;;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; Jump to function definition
(use-package dumb-jump
  :hook (prog-mode . dumb-jump-mode))

;;;; Ivy++
(use-package ivy
  :diminish ivy-mode
  :general
   (:keymaps 'ivy-minibuffer-map
   "M-j" 'ivy-next-line
   "M-k" 'ivy-previous-line
   "M-l" 'ivy-alt-done
   "M-RET" 'ivy-immediate-done
   "M-s" 'ivy-avy
   "M-h" 'ivy-backward-kill-word
   "M-d" 'ivy-scroll-down-command
   "M-u" 'ivy-scroll-up-command
   "M-p" 'yank ; For pasting passwords into the minibuffer in tramp
   )
  :init
  (setq ivy-re-builders-alist
	'((t . ivy--regex-ignore-order))) ;; Regexps can interchange order
  (setq ivy-height 20)
  (ivy-mode 1)
  :config
  (setq ivy-count-format "(%d/%d) ") ;; Proposed from the Ivy wiki
  )

(use-package counsel
  :general
  ("M-x" 'counsel-M-x
   "C-x C-f" 'counsel-find-file)
  :init
  (counsel-mode +1))


;; Show last used functions in M-x
(use-package amx)


;;;; Flycheck (linting)
(use-package flycheck)

;;;; Writegood-mode
;; Give comments on badly written text
(use-package writegood-mode
  :init (add-hook 'LaTeX-mode-hook 'writegood-mode)
  :config (delete "significantly" writegood-weasel-words))


;;;; LaTeX-stuff (AuCTeX, refTeX and more)
(use-package auctex
  :general
  (s/local-leader-def
    :keymaps 'TeX-mode-map
    "ESC" '(:ignore t :wk t)
    "v" '(TeX-view :wk "view pdf")
    "c" '(TeX-command-master :wk "compile document")
    "t" '(reftex-toc :wk "navigate document")
    "r" '(reftex-toc-Rescan :wk "refresh reftex")
    "e" '(TeX-next-error :wk "compilation errors")
    "M-f" '(LaTeX-fill-buffer :wk "fill buffer")

    "i" '(:ignore t :wk "insert")
    "i m" '(TeX-insert-macro :wk "macro")
    "i e" '(LaTeX-environment :wk "macro")
    "i ]" '(LaTeX-close-environment :wk "close environment")
    "i c" '(reftex-citation :wk "citation")
    "i r" '(reftex-reference :wk "label")

    "M-v" '(:ignore t :wk "change variables")
    "M-v f" '(s/toggle-tex-fold :wk "folding")
    "M-v v" '(s/choose-latex-pdf-viewer :wk "PDF viewer")
    )
  (:keymaps 'TeX-mode-map
   :prefix "M-i"
   "" '(:ignore t :wk "insert")
   "m" '(TeX-insert-macro :wk "macro")
   "e" '(LaTeX-environment :wk "macro")
   "]" '(LaTeX-close-environment :wk "close environment")
   "c" '(reftex-citation :wk "citation")
   "r" '(reftex-reference :wk "label")
   )
  (s/local-leader-def
    :keymaps 'bibtex-mode-map
    "t" '(org-ref-clean-bibtex-entry :wk "tidy entry")
    )

  :init

  (add-hook 'LaTeX-mode-hook 'latex-math-mode)

  ;; Completion for latex macros
  (setq 
   TeX-auto-global (concat user-emacs-directory "auctex/auto-global")
   TeX-auto-regexp-list 'TeX-auto-full-regexp-list
   TeX-macro-private "~/.local/share/latex"
   TeX-style-private "~/.local/share/latex")
  
  :config

  ;;(setq TeX-complete-expert-commands t) ; Adds more commands for completion
  (setq Tex-auto-save t) ;; Parsing on save
  (setq TeX-parse-self t) ;; Parsing on load (scan file for macros)
  (setq-default TeX-master nil) ;; Allow multi-file documents
  (setq-default TeX-PDF-mode t)

  (setq TeX-auto-regexp-list 'TeX-auto-full-regexp-list
	TeX-auto-parse-length 999999
	TeX-auto-global (concat user-emacs-directory "auctex/auto-global/"))
  
  ; (setq TeX-source-correlate-method 'synctex)
  ; (TeX-source-correlate-mode)
  ; (setq TeX-source-correlate-start-server t)
  ; (add-hook 'LaTeX-mode-hook (lambda () (TeX-source-correlate-mode t)))
  )

(use-package reftex
  :diminish reftex-mode
  :config
  (add-hook 'LaTeX-mode-hook
  	    (lambda () (reftex-mode 1)))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTEX t))

(use-package auctex-latexmk
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)

  (add-hook 'LaTeX-mode-hook
  	    (lambda ()
  	      (setq TeX-command-default "LatexMk"
  		    TeX-command-force "LatexMk")))
  :config
  (auctex-latexmk-setup)
  )

(use-package company-reftex)
(use-package company-auctex
  :config
  (defun s/latex-company-function ()
    (set (make-local-variable 'company-backends)
	 '(
	   (company-reftex-labels
	    company-reftex-citations
	    company-auctex-macros
	    company-auctex-bibs
	    company-auctex-environments
	    company-auctex-symbols
	    company-capf
	    :separate)
	   company-files
	   company-capf
	   ))
    (company-auctex-init))

  (add-hook 'LaTeX-mode-hook 's/latex-company-function)
  )


;;;; Auto-fill texts
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode 'auto-fill-mode)
(diminish 'auto-fill-function)

(diminish 'auto-fill-mode)
(diminish 'auto-revert-mode)

;;;; Dictionary (flyspell)
(use-package flyspell
  :diminish flyspell-mode
  :config

  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)

  ;; Flyspell for comments and strings in prog-mode
  (add-hook 'prog-mode 'flyspell-prog-mode)

;;  (setq-default ispell-program-name "/usr/local/bin/aspell")

  (when (executable-find "hunspell")
    (setq-default ispell-program-name "/usr/local/bin/hunspell")
    (setq ispell-really-hunspell t)
    (setenv "DICTIONARY" "english_british")
    (setq ispell-dictionary "english_british"))
  
  ;; ;; Not sure that this works
  ;; (setq-default ispell-program-name "hunspell")
  ;; (setq ispell-really-hunspell t)

  (defvar s/dictionaries
    (list
     '("british" "english_british")
     '("norwegian" "norsk_bokmaal")))

  (defun s/choose-dictionary (&optional dict-name)
    (interactive)
    (let ((dict
	   (s/choose-from-list "Select dictionary: " s/dictionaries dict-name)))
      (ispell-change-dictionary dict)))

  (s/choose-dictionary s/init-dict)

  (use-package flyspell-correct-ivy)
  )


;;;; which-key
;; Display a popup-buffer with the available key-combinations
;; whenever a keymap is pressed
(use-package which-key
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-sort-order 'which-key-local-then-key-order)
  (which-key-setup-side-window-bottom)
  (setq which-key-side-window-max-height 0.5))

;;;; Project management
(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'projectile-dired) ; go to top level directory
  )

;;;; Terminal
(use-package vterm)

;;;; Pairing of parentheses
(use-package elec-pair
  :init
  :config
  (electric-pair-mode 1)
  ;; pairing of parentheses and other symbols
  (setq-default electric-pair-pairs
		'((?\" . ?\")
		  (?\{ . ?\})
		  (?\( . ?\))
		  (?\[ . ?\])
		  ))
  ;; Do not complete if right in front of a word
  (setq-default electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (show-paren-mode) ;; Highlight matching parenthesis
  (setq-default electric-pair-skip-whitespace 'chomp) ; delete whitespace between you and closing parentheses
  )


;;;; Org-ref

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (R . t)
   (latex . t)
   (shell . t)
   (C . t)))

;; auto-fill mode
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'org-indent-mode)

(general-define-key
 :keymaps 'org-mode-map
 :states '(normal)
 "<tab>" 'org-cycle ; Evil-collection is stupid
 "<C-tab>" 'org-previous-visible-heading)

(setq org-src-tab-acts-natively t)

(s/local-leader-def
 :keymaps 'org-mode-map
 "TAB" '(org-global-cycle :wk "Cycle buffer"))

(use-package org-ref
  :init
  (setq reftex-default-bibliography (list (concat s/literature-dir "sources.bib"))
	org-ref-bibliography-notes (concat s/literature-dir "bibliography.org")
	org-ref-default-bibliography (list (concat s/literature-dir "sources.bib"))
	org-ref-pdf-directory (concat s/literature-dir "*read/"))

  (setq org-ref-completion-library 'org-ref-ivy-cite)
  
  ;; open pdf with system pdf viewer (works on mac)
  (setq bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (start-process "open" "*open*" "open" fpath)))

  (require 'org-ref)
  (require 'org-ref-latex)
  (require 'org-ref-bibtex)
  (require 'org-ref-pdf)
  (require 'org-ref-url-utils)
  (require 'doi-utils)
  (require 'org-ref-arxiv)
  (require 'org-ref-scopus)

  (add-hook 'TeX-mode-hook 'org-ref-latex-cite-on)

  (general-define-key
   :keymaps 'TeX-mode-map
   "<mouse-3>" 'org-ref-latex-click))


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

;;;; Midnight mode
(use-package midnight)


;;;; Yasnippet
(use-package yasnippet
  :init
  (yas-global-mode 1)
  (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  :general
  (:keymaps 'yas-minor-mode-map
	    "M-/" 'yas-maybe-expand)
  :config
  (add-hook 'eglot-server-initialized-hook 'yas-minor-mode))


;;;; Fix the usage of accent keys
;; https://wiki.archlinux.org/index.php/Emacs
;; "Emacs, the normal way to use accent keys does not work as expected.
;; Trying to accent a word like 'fiancé' will produce an error message."
;; We fix this with iso-transl
(require 'iso-transl)


;;; Mandatory stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes nil)
 '(ess-style 'RStudio)
 '(evil-collection-minibuffer-setup t t)
 '(evil-search-module 'evil-search))

;;; init.el ends here
