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
(load-theme 'manoj-dark)


(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non linux systems
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode)
(use-package counsel)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :config
  (ivy-mode 1))

(use-package magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

(use-package telephone-line
  :config
  (telephone-line-mode 1))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(global-command-log-mode t)
 '(package-selected-packages
   (quote
    (telephone-line doom-modeline counsel ivy command-log-mode yaml-mode use-package smartparens sage-shell-mode plsense multiple-cursors magithub lv kotlin-mode ht helm haskell-mode flymake-jslint elpy dockerfile-mode docker-tramp dash-functional conda company-plsense cider bash-completion))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


