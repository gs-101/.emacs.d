;;; -*- lexical-binding: t -*-

(use-package org
  :custom
  (org-adapt-indentation t)
  (org-auto-align-tags nil)
  (org-directory (convert-standard-filename (expand-file-name "~/Documents/Org")))
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
     (clojure . t)
     (css . t)
     (emacs-lisp . t)
     (java . t)
     (js . t)
     (latex . t)
     (python . t)
     (scheme . t)
     (shell . t)
     (sql . t)
     (sqlite . t)
     ))
  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  (nconc org-babel-default-header-args:java
         '((:dir . nil)
           (:results . "output")))
  :custom
  (org-export-babel-evaluate nil)
  )

(use-package ob-csharp
  :vc (:url "https://github.com/samwdp/ob-csharp")
  :after org
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(csharp . t))
  )

(use-package ob-http
  :vc (:url "https://github.com/ag91/ob-http")
  :after org
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(http . t))
  )

(use-package org-agenda
  :config
  (add-to-list 'org-agenda-prefix-format '(agenda . "%-12t% s "))
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
  :config
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  :defer t
  )

(use-package engrave-faces
  :after ox-latex
  :ensure t
  :custom
  (org-latex-src-block-backend 'engraved)
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
                              ))
  )

(use-package paragraphs
  :custom
  (sentence-end-double-space nil)
  :defer t
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
   ("C-z M m" . org-remark-mark)
   ("C-z M o" . org-remark-open)
   ("C-z M n" . org-remark-view-next)
   ("C-z M p" . org-remark-view-prev)
   ("C-z M DEL" . org-remark-delete)
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

(use-package fsrs
  :vc (:url "https://github.com/bohonghuang/lisp-fsrs")
  :defer t
  :ensure t
  )

(use-package org-srs
  :vc (:url "https://github.com/bohonghuang/org-srs")
  :bind
  (
   :map org-mode-map
   ("C-z m r n" . org-srs-tiem-create)
   ("C-z m r e" . org-srs-review-rate-easy)
   ("C-z m r g" . org-srs-review-rate-good)
   ("C-z m r h" . org-srs-review-rate-hard)
   ("C-z m r RET" . org-srs-review-start)
   ("C-z m r DEL" . org-srs-review-quit)
   ("C-z m r a" . org-srs-item-cloze-dwim)
   ("C-z m r k" . org-srs-item-uncloze-dwim)
   )
  :config
  (advice-add #'org-srs-item-cloze-dwim :after #'org-srs-item-cloze-update)
  (advice-add #'org-srs-item-uncloze-dwim :after #'org-srs-item-cloze-update)
  :ensure t
  )

(provide 'gs-org)
