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
