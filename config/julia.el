;;(quelpa '(julia-repl :repo "antonydellavecchia/julia-repl" :fetcher github))
(use-package julia-repl)
(julia-repl-set-terminal-backend 'vterm)
(setq julia-repl-pop-to-buffer nil)
(setq vterm-kill-buffer-on-exit nil)
(setq julia-indent-offset 2)

(cond
 ((string-equal system-name "priort")
  (setq julia-repl-executable-records
	'((default "/usr/site-local/bin/julia-latest"))))
  ((string-equal system-name "marvin")
   (setq julia-repl-executable-records
	 '((default "~/.juliaup/bin/julia")))))


(use-package julia-mode
  :mode "\\.jl\\'"
  :hook (julia-mode . smartparens-mode)
  :hook (julia-mode . company-mode)
  :hook (julia-mode . julia-repl-mode)
  :hook (julia-mode . eglot-ensure))

(use-package eglot-jl
  :ensure t)

