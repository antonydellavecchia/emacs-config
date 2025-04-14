;; -*-emacs-lisp-*-

;; add "/home/antonydv/projects/M2/M2/BUILD/build/usr-dist/common/share/emacs/site-lisp/macaulay2" to load-path if it isn't there
(add-to-list 'load-path "/home/antonydv/projects/M2/M2/BUILD/build/usr-dist/common/share/emacs/site-lisp/macaulay2")

;; add "/home/antonydv/projects/M2/M2/BUILD/build/usr-dist/common/share/info" to Info-default-directory-list if it isn't there
(add-to-list 'Info-default-directory-list "/home/antonydv/projects/M2/M2/BUILD/build/usr-dist/common/share/info")

;; add "/home/antonydv/projects/M2/M2/BUILD/build/usr-dist/x86_64-Linux-ManjaroLinux-24.1.2/bin" to PATH if it isn't there
(if (not (string-match "/home/antonydv/projects/M2/M2/BUILD/build/usr-dist/x86_64-Linux-ManjaroLinux-24.1.2/bin" (getenv "PATH")))
     (setenv "PATH" "/home/antonydv/projects/M2/M2/BUILD/build/usr-dist/x86_64-Linux-ManjaroLinux-24.1.2/bin:$PATH" t))

;; this version will give an error if M2-init.el is not found:
(load "M2-init")

;; this version will not give an error if M2-init.el is not found:
;; (load "M2-init" t)

;; You may comment out the following line with an initial semicolon if you 
;; want to use your f12 key for something else.  However, this action
;; will be undone the next time you run setup() or setupEmacs().
(global-set-key [ f12 ] 'M2)

;; Prevent Emacs from inserting a superfluous "See" or "see" in front
;; of the hyperlinks when reading documentation in Info mode.
(setq Info-hide-note-references 'hide)
