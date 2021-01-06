(setq inhibit-startup-message t)

;; disabling unwanted defaults
(scroll-bar-mode -1) 
(tool-bar-mode -1) 
(tooltip-mode -1)
(set-fringe-mode 10) ; adds space
(menu-bar-mode -1)

;; visual bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 125)

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

(require 'use-package)
(setq use-package-always-ensure t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :custom (ivy-wrap t)
  :config (ivy-mode 1))

(setq ivy-use-selectable-prompt t)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general)

(use-package magit
  :bind (("C-x g" . 'magit-status)
	 ("C-x M-g" . magit-dispatch)))

(use-package dired
  :ensure nil
  :bind (("C-x C-j" . dired-jump)))

(use-package dired-single)
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))

(use-package multiple-cursors)
(general-define-key
 "C-S-c C-S-c" 'mc/edit-lines
 "C->" 'mc/mark-next-like-this
 "C-<" 'mc/mark-previous-like-this
 "C-c C-<" 'mc/mark-all-like-this)

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
  :config (counsel-projectile-mode))

;; M-x all-the-icons-install-fonts on a new machine
(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :init (load-theme 'doom-laserwave t))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :bind (("M-p f" . sp-forward-slurp-sexp)
	 ("M-p b" . sp-backward-slurp-sexp)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1)) 

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . lsp-deferred)
  :config
  (setq js-indent-level 2))


(setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt -i")

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :bind (("M-p f" . sp-forward-slurp-sexp)))

(add-to-list 'exec-path "~/anaconda3/bin")
(setenv "PATH" "~/anaconda3/bin:$PATH" '("PATH"))
(setenv "WORKON_HOME" "/home/antony/anaconda3/envs/")

(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/anaconda3"))
  (setq conda-env-home-directory (expand-file-name "~/anaconda3"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode t))



(use-package cperl-mode
  :mode "\\.pl\\'"
  :bind (("M-p f" . sp-slurp-hybrid-sexp))
  :hook (cperl-mode . lsp-deferred)
  :config (setq electric-pair-mode nil))

(use-package kotlin-mode
  :mode "\\.kt\\'"
  :hook (kotlin-mode . lsp-deferred))

(use-package gradle-mode
  :config (gradle-mode 1))

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(counsel-describe-function-function 'helpful-callable t)
 '(counsel-describe-variable-function 'helpful-variable t)
 '(custom-enabled-themes '(wheatgrass))
 '(doom-modeline-height 15 t)
 '(gnus-select-method '(nnreddit ""))
 '(helm-completion-style 'emacs)
 '(ivy-wrap t t)
 '(js-indent-level 2)
 '(package-selected-packages
   '(all-the-icons helm-tramp docker-tramp elpygen pygen emacsql-psql emacsql conda smartparens pyenv-mode-auto pyenv-mode elpy flymake-jslint helm sphinx-doc dockerfile-mode magit treemacs circe el-get use-package flymake-json ssh-config-mode yaml-mode multiple-cursors rjsx-mode js2-mode restclient solaire-mode rubik restclient-test))
 '(projectile-completion-system 'ivy t)
 '(pyvenv-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


