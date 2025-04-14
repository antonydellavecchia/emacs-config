(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-tools-modes)
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-annot-activate-created-annotations t)
  :config
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward))


(use-package reftex
  :defer t
  :custom
  (reftex-cite-prompt-optional-args t)) ; Prompt for empty optional arguments in cite


(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-electric-math (cons "$" "$"))
  (LaTeX-electric-left-right-brace t)
  (reftex-plug-into-AUCTeX t)
  (TeX-view-program-selection '((output-pdf "Evince")))
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-start-server t))

(require 'auctex-latexmk)
(auctex-latexmk-setup)

(use-package auctex-latexmk
  :after tex
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

(use-package company-auctex
  :pin melpa
  :after tex
  :init
  (company-auctex-init))

(setq enable-local-variables nil)
