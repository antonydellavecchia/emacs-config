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
(set-frame-parameter (selected-frame) 'alpha '(75 . 100))
(add-to-list 'default-frame-alist '(alpha . (75 . 50)))

(use-package general)

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

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

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


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1)) 

(use-package company
  :ensure t
  :init
  (global-company-mode) ; Enable company-mode globally
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)  ; Load the default smartparens configuration
  (smartparens-global-mode t)    ; Enable smartparens globally
  :bind
  (("M-p f" . sp-forward-slurp-sexp)  ; Bind keys for slurping
   ("M-p b" . sp-backward-slurp-sexp))) ; and barfing

