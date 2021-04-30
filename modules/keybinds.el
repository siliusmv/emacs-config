(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree) ; Use undo-tree for undo in evil
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom (evil-collection-minibuffer-setup t)
  :config
  (evil-collection-init))

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
    :keymaps '(prog-mode-map
	       text-mode-map ess-mode-map
	       inferior-ess-mode-map
	       evil-ex-completion-map
	       minibuffer-local-map
	       swiper-map))

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
   "M-l" '(er/expand-region :wk "expand region")
   "M-h" '(er/contract-region :wk "contract region"))

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

    "c" '(makefile-executor-execute-project-target :wk "compile")

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
    "o t" '(vterm-toggle-cd :wk "terminal")
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

