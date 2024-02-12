(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

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
(load (expand-file-name "config/org.el" user-emacs-directory))
(load (expand-file-name "config/terminal.el" user-emacs-directory))
(setq initial-buffer-choice 'vterm)

(use-package eglot
  :ensure t
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (setq eglot-connect-timeout nil))

(load (expand-file-name "config/julia.el" user-emacs-directory))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode))


(use-package latex-mode
  :ensure nil
  :mode "\\.tex\\'"
  :hook (latex-mode . smartparens-mode))


(setq create-lockfiles nil)

(setq custom-file (expand-file-name "config/custom.el" user-emacs-directory))
(load custom-file 'noerror)

