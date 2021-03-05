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

