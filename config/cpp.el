(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup))

(use-package company-cmake
  :after company
  :config
  (add-to-list 'company-backends 'company-cmake))

(use-package company-cmake
  :after company
  :config
  (add-to-list 'company-backends 'company-cmake))

(use-package projectile
  :ensure t
  :config
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :compile "cmake --build Build"
                                    :test "cmake --build build --target test"
                                    :run "cmake --build build --target run"))
