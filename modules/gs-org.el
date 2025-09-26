;;; -*- lexical-binding: t -*-

(use-package org
  :custom
  (org-auto-align-tags nil)
  (org-directory (gs-101/filename "~/Documents/org"))
  (org-format-latex-options '(:foreground default
                                          :background nil
                                          :scale 1.0
                                          :html-foreground "Black"
                                          :html-background "Transparent"
                                          :html-scale 1.0
                                          :matchers
                                          ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-reverse-note-order t)
  (org-tags-column 0)
  (org-todo-repeat-to-state t)
  (org-use-sub-superscripts '{}))

(use-package org
  :config
  (gs-101/add-many-to-list 'org-babel-load-languages '((C . t)
                                                       (clojure . t)
                                                       (js . t)
                                                       (latex . t)
                                                       (python . t)
                                                       (scheme . t)
                                                       (sql . t)
                                                       (sqlite . t)))
   :custom
   (org-export-babel-evaluate nil))

(use-package ob-csharp
  :vc (:url "https://github.com/samwdp/ob-csharp")
  :after org
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(csharp . t)))

(use-package ob-dart
  :vc (:url "https://github.com/mzimmerm/ob-dart")
  :after org
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(dart . t)))

(use-package ob-http
  :vc (:url "https://github.com/ag91/ob-http")
  :after org
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(http . t)))

(use-package org-nix-shell
  :vc (:url "https://github.com/AntonHakansson/org-nix-shell")
  :ensure t
  :hook
  (org-mode . org-nix-shell-mode))

(use-package org-agenda
  :config
  (add-to-list 'org-agenda-prefix-format '(agenda . "%-12t% s "))
  :custom
  (org-agenda-custom-commands
   '(("d" "Daily Agenda"
      ((agenda ""
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
                (org-deadline-warning-days 0)))))))
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-tags-column 0)
  (org-agenda-window-setup 'only-window))

(use-package org-agenda
  :hook
  (org-agenda-mode . mlk/org-agenda-fold)
  :config
  (defun mlk/org-agenda-fold()
    "Fold headers of the agenda starting with \"* \"."
    (interactive)
    (setq-local outline-regexp "^\\* ")
    (setq-local outline-heading-end-regexp "\n")
    (setq-local outline-minor-mode-prefix (kbd "C-'"))
    (outline-minor-mode)
    (local-set-key outline-minor-mode-prefix outline-mode-prefix-map)
    (org-defkey org-agenda-mode-map [(tab)] #'outline-toggle-children)))

(use-package org-habit
  :custom
  (org-habit-graph-column 100))

(use-package org
  :after org
  :config
  (defun liron/org-hook-for-repeat-not-on-weekend ()
    "Makes repeating tasks skip weekends."
    (when (org-property-values "NO_WEEKEND")
      ;; Get time from item at POINT
      (let* ((scheduled-time (org-get-scheduled-time (point)))
             ;; Convert to timestamp - required for the next step
             (seconds-timestamp (time-to-seconds scheduled-time))
             ;; Convert to decoded time - required to find out the weekday
             (decoded-time (decode-time seconds-timestamp))
             ;; Get weekday
             (weekday (decoded-time-weekday decoded-time)))
        (when (> weekday 5) ;; Saturday -> move to Sunday
          (setq decoded-time
                (decoded-time-add decoded-time (make-decoded-time :day 2))))
        (when (> weekday 6) ;; Sunday - move to Monday
          (setq decoded-time
                (decoded-time-add decoded-time (make-decoded-time :day 1))))
        (let ((encoded-time (encode-time decoded-time)))
          (org-schedule nil encoded-time)))))
  :hook
  (org-todo-repeat . liron/org-hook-for-repeat-not-on-weekend))

(use-package org-clock
  :custom
  (org-clock-clocked-in-display 'frame-title)
  (org-clock-persist t)
  (org-clock-report-include-clocking-task t))

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
  (org-fold-catch-invisible-edits 'show-and-error))

(use-package org-cycle
  :custom
  (org-cycle-emulate-tab 'whitestart))

(use-package ox-latex
  :custom
  (org-latex-tables-centered nil)
  (org-latex-toc-command "\\tableofcontents \\pagebreak")
  (org-startup-with-latex-preview t)
  (org-preview-latex-image-directory (gs-101/filename "ltximg/" temporary-file-directory))
  :config
  (add-to-list 'org-latex-classes '("org-plain-latex"
                                    "
\\documentclass{article}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
"
                                    ("\\section{%s}" . "\\section*{%s}")
                                    ("\\subsection{%s}" . "\\subsection*{%s}")
                                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-pdf-process "bibtex %b" t))

(use-package engrave-faces
  :vc (:url "https://code.tecosaur.net/tec/engrave-faces")
  :after ox-latex
  :ensure t
  :custom
  (org-latex-src-block-backend 'engraved))

(use-package org-list
  :custom
  (org-list-allow-alphabetical t))

(use-package org
  :config
  (gs-101/add-many-to-list 'org-modules '(org-habit
                                          org-id)))

(use-package paragraphs
  :custom
  (sentence-end-double-space nil))

(use-package org-refile
  :config
  (advice-add #'org-refile :after #'org-save-all-org-buffers)
  :custom
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets '((nil :maxlevel . 1)
                        (org-agenda-files :maxlevel . 1)))
  (org-refile-use-outline-path t))

(use-package org
  :custom
  (org-todo-keywords '((sequence
                        "TODO(t)"
                        "WAIT(w!)"
                        "|"
                        "DONE(d!)"
                        "BACKLOG(b)"
                        "CANCELLED(c@)")
                       (sequence
                        "FIX(f@)"
                        "FEAT(F@)"
                        "DOCS(D@)"
                        "STYLE(s)"
                        "REFACTOR(r)"
                        "CHORE(C@)"
                        "|"
                        "MERGED(m)"
                        "CLOSED(x@)"))))

(use-package org-src
  :custom
  (org-src-window-setup 'current-window))

(use-package org-fragtog
  :vc (:url "https://github.com/io12/org-fragtog")
  :ensure t
  :hook
  (org-mode . org-fragtog-mode))

(use-package org-appear
  :vc (:url "https://github.com/awth13/org-appear")
  :custom
  (org-appear-autoentities t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  :ensure t
  :hook
  (org-mode . org-appear-mode))

(use-package org-contrib
  :after org
  :ensure t)

(use-package ox-extra
  :after org
  :config
  ;; Use the :ignore: tag to export content without the heading
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(provide 'gs-org)
