;;; -*- lexical-binding: t -*-

(use-package org
  :config
  (add-to-list 'org-latex-packages-alist '(
                                           "" "bookmark" t
                                           "" "biblatex" t
                                           "" "url" t
                                           ))
  :custom
  (org-adapt-indentation t)
  (org-auto-align-tags nil)
  (org-format-latex-options '(
                              :foreground default
                              :background nil
                              :scale 1.0
                              :html-foreground "Black"
                              :html-background "Transparent"
                              :html-scale 1.0
                              :matchers
                              ("begin" "$1" "$" "$$" "\\(" "\\[")
                              ))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-reverse-note-order t)
  (org-startup-indented t)
  (org-tags-column 0)
  (org-todo-repeat-to-state t)
  (org-use-sub-superscripts '{})
  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  )

(use-package startup
  :custom
  (initial-major-mode 'org-mode)
  :defer t
  )

(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (C . t)
     (css . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (js . t)
     (latex . t)
     (python . t)
     (shell . t)
     ))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  (add-hook 'after-save-hook 'org-babel-tangle)
  )

(use-package org-agenda
  :custom
  (org-agenda-custom-commands
   '((
      "d" "Daily Agenda"
      (
       (agenda ""
               ((org-agenda-overriding-header "* High Priority Tasks")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "\#A"))
                (org-agenda-span 'day)
                (org-deadline-warning-days 0)))

       (agenda ""
               ((org-agenda-overriding-header "* Medium Priority Tasks")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "\#B"))
                (org-agenda-span 'day)
                (org-deadline-warning-days 0)))

       (agenda ""
               ((org-agenda-overriding-header "* Low Priority Tasks")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "\#C"))
                (org-agenda-span 'day)
                (org-deadline-warning-days 0)))
       )
      )
     ))
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-tags-column 0)
  (org-agenda-window-setup 'only-window)
  )

(use-package org-agenda
  :hook
  (org-agenda-mode . mlk/org-agenda-fold)
  :preface
  (defun mlk/org-agenda-fold()
    "Fold headers of the agenda starting with \"* \"."
    (interactive)
    (setq-local outline-regexp "^\\* ")
    (setq-local outline-heading-end-regexp "\n")
    (setq-local outline-minor-mode-prefix (kbd "C-'"))
    (outline-minor-mode)
    (local-set-key outline-minor-mode-prefix outline-mode-prefix-map)
    (org-defkey org-agenda-mode-map [(tab)] #'outline-toggle-children)
    )
  )

(use-package org-habit
  :custom
  (org-habit-graph-column 100)
  )

(use-package org-clock
  :custom
  (org-clock-clocked-in-display 'frame-title)
  (org-clock-persist t)
  (org-clock-report-include-clocking-task t)
  )

(use-package org-compat
  :config
  (org-add-link-type
   "youtube"
   (lambda (handle)
     (browse-url (concat "https://www.youtube.com/watch?v=" handle)))
   (lambda (path desc backend)
     (cl-case backend
       (html (format
              "<p style='text-align:center;'>
<iframe width='420' height='315' align='middle'
src='https://youtube.com/embed/W4LxHn5Y_l4?controls=0'
allowFullScreen>
</iframe>
</p>"
              path (or desc "")))
       (latex (format "\href{%s}{%s}" path (or desc "video"))))))

  :custom
  (org-fold-catch-invisible-edits 'show-and-error)
  )

(use-package org-cycle
  :custom
  (org-cycle-emulate-tab 'whitestart)
  )

(use-package ox-latex
  :custom
  (org-latex-tables-centered nil)
  (org-latex-toc-command "\\tableofcontents \\pagebreak")
  (org-startup-with-latex-preview t)
  (org-preview-latex-default-process 'dvisvgm) ;; 1
  (org-preview-latex-image-directory (convert-standard-filename (expand-file-name "ltximg/" temporary-file-directory)))
  :defer t
  )

(use-package org-list
  :custom
  (org-list-allow-alphabetical t)
  )

(use-package org
  :config
  (add-to-list 'org-modules '(
                              org-habit
                              org-id
                              org-protocol
                              )
               )
  )

(use-package org-refile
  :config
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  :custom
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets '(
                        (nil :maxlevel . 1)
                        (org-agenda-files :maxlevel . 1)
                        ))
  (org-refile-use-outline-path t)
  )

(use-package org
  :custom
  (org-tag-alist '(
                   ;; Places
                   (:startgroup)
                   ("Places")
                   (:grouptags)
                   ("@home" . ?h)
                   (:endgroup)

                   ;; Contexts
                   (:startgroup)
                   ("Contexts")
                   (:grouptags)
                   ("@computer" . ?c)
                   ("@mobile" . ?m)
                   (:endgrouptag)

                   ;; Task Types
                   (:startgroup)
                   ("Types")
                   (:grouptags)
                   ("@hacking" . ?H)
                   ("@writing" . ?w)
                   ("@creative" . ?C)
                   ("@accounting" . ?a)
                   ("@email" . ?e)
                   ("@system" . ?s)
                   (:endgrouptag)

                   ;; Workflow states
                   (:startgroup)
                   ("States")
                   (:grouptags)
                   ("@plan" . ?p)
                   ("@review" . ?r)
                   (:endgroup)
                   ))
  )

(use-package org
  :custom
  (org-todo-keywords '(
                       (sequence
                        "TODO(t)"
                        "WRITE(W)"
                        "WAIT(w!)"
                        "|"
                        "DONE(d!)"
                        "BACKLOG(b)"
                        "CANCELLED(c@)"
                        )
                       (sequence
                        "GOAL(g)"
                        "PROJ(p)"
                        "|"
                        "DONE(d!)"
                        )
                       (sequence
                        "FIX(f@)"
                        "FEAT(F@)"
                        "STYLE(s)"
                        "REFACTOR(r)"
                        "CHORE(C@)"
                        "|"
                        "MERGED(m)"
                        "CLOSED(x@)"
                        )
                       ))
  )

(use-package org-src
  :custom
  (org-src-window-setup 'current-window)
  )

(use-package org-alert
  :ensure t
  )

(use-package org-fragtog
  :ensure t
  :hook
  (org-mode . org-fragtog-mode)
  )

(use-package org-appear
  :custom
  (org-appear-autoentities t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  :ensure t
  :hook
  (org-mode . org-appear-mode)
  )

(use-package org-remark
  :bind
  (
   :map org-remark-mode-map
   ("C-c M m" . org-remark-mark)
   ("C-c M o" . org-remark-open)
   ("C-c M ]" . org-remark-view-next)
   ("C-c M [" . org-remark-view-prev)
   ("C-c M d" . org-remark-delete)
   )
  :ensure t
  :config
  (org-remark-global-tracking-mode)
  )

(use-package org-remark-eww
  :hook
  (eww-mode . org-remark-eww-mode)
  )

(use-package org-remark-info
  :hook
  (info-mode . org-remark-info-mode)
  )

(use-package org-remark
  :hook
  (nov-mode . org-remark-nov-mode)
  )

(use-package toc-org
  :ensure t
  :hook
  (org-mode . toc-org-mode)
  (markdown-mode . toc-org-mode)
  )

(use-package website2org
  :vc (:url "https://github.com/rtrppl/website2org")
  :custom
  (website2org-cache-filename "/tmp/website2org")
  :ensure t
  )

(use-package website2org
  :after website2org org-roam
  :custom
  (website2org-directory (expand-file-name "website2org/" org-roam-directory))
  (website2org-additional-meta "#+filetags: :website2org:")
  )

(provide 'gs-org)
