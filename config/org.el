(use-package org)

(setq org-directory "/home/antonydv/org")
(setq org-startup-indented t)
(setq org-src-fontify-natively t)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; learn about agenda?
;;(setq org-agenda-files (list "~/path/to/your/notes"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "/home/antonydv/org/todos.org" "Todos")
         "* TODO %?\n  %i\n  %a\n  [[%F][Link back to note]]")
        ("n" "Note" entry (file+headline "/home/antonydv/notes.org" "Notes")
         "* %? \nEntered on %U\n  %i\n  %a")))




