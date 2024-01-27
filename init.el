
(setq indent-tabs-mode nil)
(setq js-indent-level 2)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non linux systems
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(load (expand-file-name "config/ui.el" user-emacs-directory) 'noerror)

(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)



(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package typescript-mode)
(setq typescript-indent-level 2)


(use-package flymake-json
  :hook (json-mode . flymake-json-load))


(use-package glsl-mode)

(use-package cperl-mode
  :mode "\\.pl\\'")


(use-package zmq)
(load (expand-file-name "config/terminal.el" user-emacs-directory))


(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(setq julia-repl-executable-records
      '((default "~/.juliaup/bin/julia")                  ; in the executable path
        ))

(use-package eglot-jl
  :init
  (setq eglot-stay-out-of '(flymake)))
(setq eglot-connect-timeout nil)

(setq eglot-jl-julia-command "~/.juliaup/bin/julia")

;;(quelpa '(julia-repl :repo "antonydellavecchia/julia-repl" :fetcher github))
(use-package julia-repl)
(julia-repl-set-terminal-backend 'vterm)
(setq julia-repl-pop-to-buffer nil)
(setq vterm-kill-buffer-on-exit nil)

(use-package julia-mode
  :mode "\\.jl\\'"
  :hook (julia-mode . smartparens-mode)
  :hook (julia-mode . company-mode)
  ;;:hook (julia-mode . lsp-mode)
  :hook (julia-mode . julia-repl-mode))

(use-package latex-mode
  :ensure nil
  :mode "\\.tex\\'"
  :hook (latex-mode . smartparens-mode))

(use-package org)
(setq create-lockfiles nil)

(setq initial-buffer-choice 'vterm)

(setq custom-file (expand-file-name "config/custom.el" user-emacs-directory))
(load custom-file 'noerror)

