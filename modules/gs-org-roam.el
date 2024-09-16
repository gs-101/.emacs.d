(use-package org-roam
  :bind
  ("C-c r f" . org-roam-node-find)
  ("C-c r i" . org-roam-node-insert)
  :custom
  (org-roam-completion-everywhere t)
  :demand t
  :ensure t
  :init
  (org-roam-db-autosync-mode)
  )

(use-package org-roam
  :custom
  (org-roam-capture-templates '(
                                ("b" "book notes" plain
                                 (file "~/org-roam/templates/book_note.org")
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :book_notes:\n\n")
                                 :unnarrowed t)
                                ("d" "default" plain
                                 (file "~/org-roam/templates/default.org") 
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n\n")
                                 :unnarrowed t)
                                ("n" "notegpt.io" plain
                                 (file "~/org-roam/templates/notegpt.io.org")
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :notegpt_io:hacker_news:\n\n")
                                 :unnarrowed t)
                                ("r" "redação" plain
                                 (file "~/org-roam/templates/redação.org")
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :redação:\n\n")
                                 :unnarrowed t)
                                ("s" "summarize.ing" plain
                                 (file "~/org-roam/templates/summarize.ing.org")
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :summarize_ing:\n\n")
                                 :unnarrowed t)
                                ))
  )

(use-package org-roam-dailies
  :bind-keymap
  ("C-c r d" . org-roam-dailies-map)
  :bind 
  (
   :map org-roam-dailies-map
   ("Y" . org-roam-dailies-capture-yesterday)
   ("T" . org-roam-dailies-capture-tomorrow)
   )
  :custom
  (dw/daily-note-filename "%<%Y-%m-%d>.org")
  (dw/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
  :demand t
  )

(use-package org-roam-dailies
  :custom
  (org-roam-dailies-capture-templates '(
                                        ("d" "default" entry
                                         "* %?"
                                         :if-new (file+head ,dw/daily-note-filename
                                                            ,dw/daily-note-header))
                                        ("t" "task" entry
                                         "* TODO %?\n  %U\n  %a\n  %i"
                                         :if-new (file+head+olp ,dw/daily-note-filename
                                                                ,dw/daily-note-header
                                                                ("Tasks"))
                                         :empty-lines 1)
                                        ("l" "log entry" entry
                                         "* %<%I:%M %p> - %?"
                                         :if-new (file+head+olp ,dw/daily-note-filename
                                                                ,dw/daily-note-header
                                                                ("Log")))
                                        ("j" "journal" entry
                                         "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
                                         :if-new (file+head+olp ,dw/daily-note-filename
                                                                ,dw/daily-note-header
                                                                ("Log")))
                                        ("m" "meeting" entry
                                         "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
                                         :if-new (file+head+olp ,dw/daily-note-filename
                                                                ,dw/daily-note-header
                                                                ("Log")))
                                        ))
  )

(use-package org-agenda
  :bind
  ("C-c r b" . dw/org-roam-capture-inbox)
  :preface
  (defun dw/org-roam-filter-by-tag (tag-name)
    "Filter org roam files by their tags."
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))
  
  (defun dw/org-roam-list-notes-by-tag (tag-name)
    "List org roam files by their tags."
    (mapcar #'org-roam-node-file
            (seq-filter
             (dw/org-roam-filter-by-tag tag-name)
             (org-roam-node-list))))
  
  (defun dw/org-roam-refresh-agenda-list () ;; 1
    "Refresh the current agenda list, and add the files with the currosponding tag to the agenda list."
    (interactive)
    (setq org-agenda-files (dw/org-roam-list-notes-by-tag "agenda")))
  ;; Build the agenda list the first time for the session
  (dw/org-roam-refresh-agenda-list)
  (defun dw/org-roam-project-finalize-hook ()
    "Adds the captured project file to "org-agenda-file" if the capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'dw/org-roam-project-finalize-hook)
    
    ;; Add project file to the agenda list if the capture was confirmed
    (unless org-note-abort
      (with-current-buffer (org-capture-get :buffer)
        (add-to-list 'org-agenda-files (buffer-file-name)))))
  (defun dw/org-roam-capture-inbox ()
    "Create a org roam inbox file."
    (interactive)
    (org-roam-capture- :node (org-roam-node-create)
                       :templates '(("i" "inbox" plain "* %?"
                                     :if-new (file+head "inbox.org" "#+title: Inbox\n#+filetags: :agenda:\n\n")))))
  (defun dw/org-roam-goto-month ()
    "Lists the files of the selected month with the set tag."
    (interactive)
    (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
                       :node (org-roam-node-create)
                       :templates '(("m" "month" plain "\n* Goals\n\n%?* Summary\n\n"
                                     :if-new (file+head "%<%Y-%B>.org"
                                                        "#+title: %<%Y-%B>\n#+filetags: :agenda:\n\n")
                                     :unnarrowed t))))
  (defun dw/org-roam-goto-year ()
    "Lists the files of the selected year with the set tag."
    (interactive)
    (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
                       :node (org-roam-node-create)
                       :templates '(("y" "year" plain "\n* Goals\n\n%?* Summary\n\n"
                                     :if-new (file+head "%<%Y>.org"
                                                        "#+title: %<%Y>\n#+filetags: :agenda:\n\n")
                                     :unnarrowed t))))
  :custom
  (org-agenda-hide-tags-regexp "agenda")
  )

(use-package org-roam-ui
  :bind
  ("C-c r u" . org-roam-ui-open)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  (org-roam-ui-browser-function #'browse-url-chromium)
  :ensure t
  )

(provide 'gs-org-roam)
