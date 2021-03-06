
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


;;;; Project management
(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'projectile-dired) ; go to top level directory
  )

