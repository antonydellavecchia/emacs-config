(add-to-list 'package-selected-packages 'dash)
(add-to-list 'package-selected-packages 'lsp-mode)
(add-to-list 'package-selected-packages 'magit-section)

(quelpa '(lean4-mode :repo "leanprover-community/lean4-mode" :fetcher github))

(add-to-list 'load-path (expand-file-name "quelpa/build/lean4-mode" user-emacs-directory))
(require 'lean4-mode)
