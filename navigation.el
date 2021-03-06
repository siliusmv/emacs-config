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


;;;; Jump to function definition
(use-package dumb-jump
  :hook (prog-mode . dumb-jump-mode))

