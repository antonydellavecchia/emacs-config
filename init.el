(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq indent-tabs-mode nil)
(setq tab-width 2)
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

(use-package protobuf-mode)
(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

(use-package tikz)
(setq tikz-viewer "evince")

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))


(load (expand-file-name "config/mrdi.el" user-emacs-directory))

(use-package flymake-json
  :hook (json-mode . flymake-json-load))

(use-package glsl-mode)

(use-package cperl-mode
  :mode "\\.pl\\'")


(use-package zmq)
(load (expand-file-name "config/org.el" user-emacs-directory))
(load (expand-file-name "config/terminal.el" user-emacs-directory))
(use-package vterm)

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents   . 5)
                        (projects  . 5)
                        ;;(agenda    . 5)
                        ;;(registers . 5)
			))
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  dashboard-insert-navigator
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  dashboard-insert-items
                                  dashboard-insert-newline
                                  dashboard-insert-footer))
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice
      (lambda () (get-buffer-create dashboard-buffer-name)
	(dashboard-refresh-buffer)
	))


(load (expand-file-name "config/cpp.el" user-emacs-directory))
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


(load (expand-file-name "config/latex.el" user-emacs-directory))
(load (expand-file-name "config/macaulay2.el" user-emacs-directory))
(load (expand-file-name "config/lean.el" user-emacs-directory))

(setq create-lockfiles nil)

(setq custom-file (expand-file-name "config/custom.el" user-emacs-directory))
(load custom-file 'noerror)


