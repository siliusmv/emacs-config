
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  ;; Use tre-sitter instead of font-lock (https://ubolonton.github.io/emacs-tree-sitter/syntax-highlighting/)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  )

(use-package tree-sitter-langs
  ;; I thought this contained syntax for R, but I was wrong, so it is most likely completely useless... This and tree-sitter
  )
