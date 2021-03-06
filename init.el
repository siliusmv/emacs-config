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

;; Load evil and general.el and define all your personal keybindings.
;; Use which-key to keep control of all the bindings
;; This needs to be quite early
(load-user-file "keybinds.el")

;; Use company-mode for completion, eglot with flymake for languageservers,
;; flyspell for spelling, yasnippet and ivy/counsel
(load-user-file "completion.el")

;; All my auctex-stuff and reftex stuff
(load-user-file "latex.el")

;; Everything needed for R, julia, elisp and others?
(load-user-file "programming.el")

;; My themes and modeline and code for changing text size etc.
(load-user-file "visual.el")

;; Some configuration for org-mode and org-ref
(load-user-file "org.el")

;; Dired, avy, ace-window, dumb-jump
(load-user-file "navigation.el")

;; Workspaces, projects, ways to separate between code
(load-user-file "projects.el")

;; Everything else
(load-user-file "packages.el")



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
