;;;; ESS (Emacs Speaks Statistics)
(use-package ess
  :init
  (require 'ess-r-mode)
  (defun s/ess-r-company ()
    "Set company backends for R buffers"
    (set (make-local-variable 'company-backends)
	 '(company-capf
	   company-files
	   company-dabbrev-code
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

;;;; Elisp stuff
(s/local-leader-def
 :keymaps 'emacs-lisp-mode-map
 "o" '(eval-defun :wk "evaluate outer sexp")
 "i" '(eval-last-sexp :wk "evaluate inner sexp")
 )

