
;;;; Ivy++
(use-package ivy
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



(use-package company
  :delight
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (general-define-key
   :keymaps 'company-active-map
   "TAB" 'company-select-next
   "<backtab>" 'company-select-previous
   "S-TAB" 'company-select-previous
   "<return>" nil
   "M-l" 'company-complete
   "M-j" 'company-select-next
   "M-k" 'company-select-previous
   "M-n" 'company-other-backend
   "M-/" 'counsel-company
   "M-S" '(counsel-company :wk "counsel-company"))

  (general-define-key
   :keymaps 'company-search-map
   "M-j" 'company-search-repeat-forward
   "M-k" 'company-search-repeat-backward)

  (general-define-key
   :states 'insert
   :keymaps 'company-mode-map
   "TAB" 'company-indent-or-complete-common
   "M-n" 'company-complete)

  ;; set default `company-backends'
  (setq company-backends
	'(company-capf
	  company-files ; files & directory
	  company-dabbrev-code
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

;;;; Flycheck (linting)
(use-package flycheck)

;;;; Writegood-mode
;; Give comments on badly written text
(use-package writegood-mode
  :init (add-hook 'LaTeX-mode-hook 'writegood-mode)
  :config (delete "significantly" writegood-weasel-words))

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

