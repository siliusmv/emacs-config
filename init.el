;;; My Emacs init file

;;;; TODO
;; FIX YASNIPPET
;; USE MULTIPLE FILES FOR YOUR CONFIG
;; Ensure that the dictionary in auctex is correct, and not "default"
;; Figure out how to control the kill-ring
;; Add expand-region
;; Speed up eglot in some way?
;; Fix the autosave-stuff!

;; Define the directory with all init-files
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/"))
  "Directory containing all user-specific init-files")

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;;; Non-package specific stuff
;;;; Global variables and constants
(defvar s/init-theme "light") ; Default theme
(defvar s/init-dict "british") ; Default language
(defvar s/literature-dir "~/OneDrive - NTNU/literature/")
(setq-default fill-column 100) ; Column for starting automatic line wrap
(setq-default default-input-method "TeX") ; Input method activated by the command toggle-input-method

;; Fixes garbage collection, bootstraps straight.el and
;; sets some settings that are nice to have
(load-user-file "startup.el")

;; Some random functions that make life easier
(load-user-file "functions.el")

;; Load evil and general.el and define all your personal keybindings
(load-user-file "keybinds.el")

;; Use company-mode for completion
(load-user-file "completion.el")

;;; Package specific settings

;;;; Diminish
;; Don't let active modes clutter the mode-line
;; You kind of don't need this when you have the evil-modeline...
(use-package diminish)

;;;; Smooth scrolling
;; Keep the pointer in the center of the screen
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

;;;; Change text size
;; This contains the functions default-text-scale-(increase/decrease)
(use-package default-text-scale)

;;;; Dired stuff

(defun s/dired-hide-dotfiles ()
  "Hides all dotfiles in a dired-buffer"
  (interactive)
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
   ;; Try to make eglot easier on the CPU
   eglot-ignored-server-capabilites
   '(:hoverProvider)
   ; :definitionProvider :typeDefinitionProvider
   ; :implementationProvider :implementationProvider
   ; :referencesProvider :documentHighlightProvider
   ; :documentSymbolProvider :workspaceSymbolProvider
   ; :codeLensProvider :documentFormattingProvider
   ; :documentRangeFormattingProvider :documentLinkProvider)
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
  (add-hook 'julia-mode-hook 'julia-repl-mode)
  ;; (add-hook 'julia-mode-hook 'julia-snail-mode)
  ;; (add-hook 'julia-mode-hook 'yas-minor-mode) ;; I don't know why I had this here...
  )
;; 
;; ;; You have to go into the source code of the function
;; ;; eglot-jl--ls-invocation and comment out the line where
;; ;; they change the environment variable JULIA_LOAD_PATH
;; ;; for this to not destroy everything
;; (use-package eglot-jl
;;   :init
;;   (setq eglot-jl-default-environment "~/.julia/environments/v1.4")
;;   (eglot-jl-init)
;;   :config
;;   (add-hook 'julia-mode-hook 'eglot-ensure))
;; 
(use-package julia-repl
  :general
  (:keymaps 'julia-repl-mode-map
   "M-e" 'julia-repl-send-line
   "M-RET" 'julia-repl-send-region-or-line)
  (s/local-leader-def
    :keymaps '(julia-repl-mode-map)
    "j" '(julia-repl :wk "Open julia repl")
    "a" '(julia-repl-activate-parent :wk "Activate project in parent directories")
    "d" '(julia-repl-doc :wk "Documentation")
    "r" '(julia-repl-includet-buffer :wk "Revise run buffer"))
)
;; 
;; (use-package julia-snail
;;   :init
;;   (defun s/julia-snail-send-line-and-step ()
;;     (interactive)
;;     (julia-snail-send-line)
;;     (evil-next-line))
;;   :general
;;   (:keymaps 'julia-snail-mode-map
;;    "M-e" 's/julia-snail-send-line-and-step
;;    "M-RET" 'julia-snail-send-top-level-form)
;;   (s/local-leader-def
;;     :keymaps 'julia-mode-map
;;     "j" '(julia-snail :wk "julia"))
;;   )

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
  (counsel-mode +1)
  )

;; Show last used functions in M-x
(use-package amx)


;;;; Flycheck (linting)
(use-package flycheck)

;;;; Writegood-mode
;; Give comments on badly written text
(use-package writegood-mode
  :init (add-hook 'LaTeX-mode-hook 'writegood-mode)
  :config (delete "significantly" writegood-weasel-words))

;;;; Compilation
(use-package makefile-executor)


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
  )

(use-package reftex
  :diminish reftex-mode
  :config
  (add-hook 'LaTeX-mode-hook (lambda () (reftex-mode 1)))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTEX t))

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

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode))

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

(use-package vterm-toggle
  :config
  ;; Show vterm buffer in side window (taken from https://github.com/jixiuf/vterm-toggle)
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  :general
  (s/local-leader-def
    :keymaps '(vterm-mode-map vterm-copy-mode-map)
    "c" '(vterm-toggle-insert-cd :wk "cd to last buffer"))
  )

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
