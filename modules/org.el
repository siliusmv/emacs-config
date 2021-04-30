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

