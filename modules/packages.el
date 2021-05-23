
;;;; Smooth scrolling
;; Keep the pointer in the center of the screen
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

;;;; Undo-tree
(use-package undo-tree
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

;;;; Compilation
(use-package makefile-executor)

;;;; Auto-fill texts
(add-hook 'text-mode-hook 'auto-fill-mode)


(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode))


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

;;;; Midnight mode
(use-package midnight)

;;;; Fix the usage of accent keys
;; https://wiki.archlinux.org/index.php/Emacs
;; "Emacs, the normal way to use accent keys does not work as expected.
;; Trying to accent a word like 'fiancé' will produce an error message."
;; We fix this with iso-transl
(require 'iso-transl)


