
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

(add-hook 'LaTeX-mode 'auto-fill-mode)
