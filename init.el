(require 'package)
(package-initialize)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (wheatgrass)))
 '(gnus-select-method (quote (nnreddit "")))
 '(helm-completion-style (quote emacs))
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (kotlin-mode bash-completion plsense cider company-plsense sage-shell-mode magithub helm-tramp docker-tramp lsp-mode ac-js2 auctex elpygen pygen ac-haskell-process haskell-mode git-blamed emacsql-psql emacsql conda smartparens nnreddit pyenv-mode-auto pyenv-mode elpy flymake-jslint helm sphinx-doc dockerfile-mode magit treemacs indium leetcode circe el-get use-package slack flymake-json ssh-config-mode yaml-mode multiple-cursors rjsx-mode js2-mode restclient solaire-mode rubik restclient-test)))
 '(pyvenv-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(require 'smartparens-config)
(smartparens-mode 1)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'cperl-mode-hook #'smartparens-mode)
(add-hook 'lisp-mode-hook #'smartparens-mode)
(global-set-key (kbd "M-p f") 'sp-forward-slurp-sexp)
(global-set-key (kbd "M-p b") 'sp-backward-slurp-sexp)

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(put 'dockerfile-image-name 'safe-local-variable #'stringp)

(require 'flymake-jslint)
(add-hook 'js-mode-hook 'flymake-jslint-load)

(require 'helm)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)


(require 'docker-tramp)


(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)


;; elpy
(package-initialize)
(elpy-enable)


(setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt -i")


(setenv "WORKON_HOME" "/home/antony/anaconda3/envs/")

;; conda.el configuration begins
(require 'conda)
;; if you want interactive shell support, include:
(conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
(conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
(conda-env-autoactivate-mode t)
;; from https://stackoverflow.com/a/56722815
(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/anaconda3"))
  (setq conda-env-home-directory (expand-file-name "~/anaconda3")))

(add-to-list 'exec-path "~/anaconda3/bin") (setenv "PATH" "~/anaconda3/bin:$PATH" '("PATH"))
;; conda.el configuration ends

(define-key python-mode-map (kbd "C-c i") 'elpygen-implement)

(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

(defun shk-eshell-prompt ()
  (let ((header-bg "#000"))
    (concat
     (with-face (concat (eshell/pwd) " ") :background header-bg :foreground "green")
     (with-face "\n" :background header-bg)
     ""
     (with-face (if (not conda-env-current-name)
                    ""
                  (concat "(" conda-env-current-name ")"))
                :background header-bg :foreground "green")
     (with-face "" :foreground "green")
     (if (= (user-uid) 0)
	 (with-face " #" :foreground "red")
       (with-face "ϕ" :foreground "red"))
     " ")))
(setq eshell-prompt-function 'shk-eshell-prompt)
(setq eshell-highlight-prompt nil)

;; dockerfile-mode
(setq dockerfile-mode-command "docker")
(put 'dockerfile-image-name 'safe-local-variable #'stringp)
;; docker enda

(global-set-key (kbd "C-c C-s") 'slack-all-unreads)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

(setq-default indent-tabs-mode nil)

(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(setq js2-strict-missing-semi-warning nil)
(setq js2-mode-show-strict-warnings nil)

;;sql mode
(defun upcase-sql-keywords ()
  (interactive)
  (save-excursion
    (dolist (keywords sql-mode-postgres-font-lock-keywords)
      (goto-char (point-min))
      (while (re-search-forward (car keywords) nil t)
        (goto-char (+ 1 (match-beginning 0)))
        (when (eql font-lock-keyword-face (face-at-point))
          (backward-char)
          (upcase-word 1)
          (forward-char))))))

(put 'upcase-region 'disabled nil)

(defalias 'perl-mode 'cperl-mode)
(require 'plsense)

;; Key binding
(setq plsense-popup-help-key "C-:")
(setq plsense-display-help-buffer-key "M-:")
(setq plsense-jump-to-definition-key "M-.")

(add-to-list 'company-backends 'company-plsense)
(add-hook 'cperl-mode-hook 'company-mode)

