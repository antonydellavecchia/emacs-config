(setq inhibit-startup-message t)

;; disabling unwanted defaults
(scroll-bar-mode -1) 
(tool-bar-mode -1) 
(tooltip-mode -1)
(set-fringe-mode 10) ; adds space
(menu-bar-mode -1)
(indent-tabs-mode nil)
(setq js-indent-level 2)
;; visual bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 125)
(set-frame-parameter (selected-frame) 'alpha '(75 . 10))
(add-to-list 'default-frame-alist '(alpha . (75 . 50)))

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

(use-package counsel)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :custom (ivy-wrap t)
  :config (ivy-mode 1))

(setq ivy-use-selectable-prompt t)

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

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

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


;;(require 'yasnippet)
;;(yas-global-mode 1)
;;(add-hook 'yas-minor-mode-hook ( lambda ()
;;				 (yas-activate-extra-mode 'fundamental-mode)))

(use-package flymake-json
  :hook (json-mode . flymake-json-load))

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

;; M-x all-the-icons-install-fonts on a new machine
(use-package all-the-icons
  :ensure t)

(use-package ivy-rich
  :ensure t
  :after (ivy))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

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


(use-package glsl-mode)


(use-package cperl-mode
  :mode "\\.pl\\'")


(use-package company
  :hook (prog-mode . company-mode)
  :hook (latex-mode . company-mode)
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection))
  ;;(:map lsp-mode-map
	;;("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package vterm
  :ensure t
  :commands vterm
  :bind (:map vterm-mode-map
         ("C-x C-f" . (lambda (&rest _)
                        (interactive)
                        (my/vterm-directory-sync)
                        ;; NOTE
                        ;; ido/helm also works
                        (call-interactively 'counsel-find-file))))
  :config
  (setq vterm-max-scrollback 1000)
  (defun my/vterm-directory-sync ()
    "Synchronize current working directory."
    (when vterm--process
      (let* ((pid (process-id vterm--process))
             (dir (file-truename (format "/proc/%d/cwd" pid))))
        (setq-local default-directory dir))))
  )

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(setq julia-repl-executable-recorsd
      '((default "~/.juliaup/bin/julia")                  ; in the executable path
        ))

(use-package eglot-jl
  :init
  (setq eglot-stay-out-of '(flymake)))

;;(setq eglot-jl-julia-command "~/.juliaup/bin/julia")

(use-package julia-repl)
(julia-repl-set-terminal-backend 'vterm)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-safe-themes
   '("3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" "a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" default))
 '(global-command-log-mode t)
 '(line-number-mode nil)
 '(package-selected-packages
   '(evil editorconfig yafolding flymake-json json-mode ejson-mode git-blamed julia-mode julia-repl clang-format glsl-mode eglot-jl julia-mode vterm dired-single counsel-projectile projectile general doom-modeline ivy-rich doom-themes helpful which-key rainbow-delimiters multiple-cursors counsel ivy yaml-mode use-package smartparens plsense))
 '(safe-local-variable-values '((projectile-project-run-cmd . "npm start")))
 '(warning-suppress-types '((emacs) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "spring green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "CTDB" :family "Fira Code"))))
 '(cursor ((t (:background "green"))))
 '(font-lock-type-face ((t (:foreground "lime green")))))


