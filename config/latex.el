(use-package tex
  :defer t
  :ensure auctex)

(use-package pdf-tools
  :ensure t)
(setq TeX-view-program-selection '((output-pdf "Evince")))
; use synctex for click on pdf to jump to that point in tex file, and vice versa:
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)

(use-package LaTeX-mode
  :ensure nil
  :mode "\\.tex\\'"
  :hook (latex-mode . smartparens-mode))

