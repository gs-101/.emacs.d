;;; -*- lexical-binding: t -*-

(use-package apropos
  :bind
  ("<f1> a" . apropos)
  ("<f1> A" . apropos-documentation)
  )

(use-package custom
  :bind
  ("<f1> t" . load-theme)
  )

(use-package descr-text
  :bind
  ("<f1> '". describe-char)
  )

(use-package compile
  :bind
  ("C-c c c" . compile)
  ("C-c c C" . recompile)
  )

(use-package eglot
  :bind
  (
   :map prog-mode-map
   ("C-c c t e" . eglot)
   :map eglot-mode-map
   ("C-c c a" . eglot-code-actions)
   ("C-c c o" . eglot-code-action-organize-imports)
   ("C-c c i" . eglot-find-implementation)
   ("C-c c t" . eglot-find-typeDefinition)
   ("C-c c f" . eglot-format)
   ("C-c c r" . eglot-rename)
   )
  )

(use-package elisp-mode
  :bind
  (
   :map emacs-lisp-mode-map
        ("C-c C-b" . elisp-byte-compile-buffer)
        ("C-c l e d" . eval-defun)
        ("C-c C-e" . elisp-eval-region-or-buffer)
        ("C-c l e e" . eval-last-sexp)
        )
  )

(use-package emacs
  :bind
  (
   :map emacs-lisp-mode-map
   ("C-c l e b" . eval-buffer)
   ("C-c l e r" . eval-region)
   )
  )

(use-package files
  :bind
  (
   :map  emacs-lisp-mode-map
         ("C-c l e l" . load-library)
         )
  )

(use-package find-func
  :bind
  (
   :map emacs-lisp-mode-map
   ("C-c l g f" . find-function)
   ("C-c l g l" . find-library)
   ("C-c l g v" . find-variable)
   )
  )

(use-package flymake
  :bind
  (
   :map flymake-mode-map
   ([remap next-error] . flymake-goto-next-error)
   ([remap previous-error] . flymake-goto-prev-error)
   :map project-prefix-map
   ("t f" . flymake-show-project-diagnostics)
   :map prog-mode-map
   ("C-c t f" . flymake-start)
   )
  )

(use-package electric
  :bind
  ("C-j" . electric-newline-and-maybe-indent)
  )

(use-package emacs
  :bind
  ("C-c q f" . delete-frame)
  ("C-c c e" . eval-buffer)
  ("C-c c E" . eval-region)
  ("C-c i c" . insert-char)
  )

(use-package emoji
  :bind
  ("C-c i e" . emoji-insert)
  )

(use-package files
  :bind
  ("C-c q K" . save-buffers-kill-emacs)
  ("C-c f r" . recover-this-file)
  ("C-c b r" . revert-buffer)
  )

(use-package find-func
  :bind
  ("<f1> P" . find-library)
  )

(use-package grep
  :bind
  ("C-c g g" . grep)
  ("C-c g l" . lgrep)
  ("C-c g r" . rgrep)
  )

(use-package help
  :bind
  ("<f1> M" . describe-minor-mode)
  ("C-h C-b" . describe-prefix-bindings)
  )

(use-package help-fns
  :bind
  ("<f1> F" . describe-face)
  )

(use-package ibuffer
  :bind
  ("C-x B" . ibuffer)
  )

(use-package imenu
  :bind
  ("C-'" . imenu)
  )

(use-package isearch
  :bind
  ("C-s" . isearch-forward-word)
  :custom
  (lazy-highlight-initial-delay 0.0)
  )

(use-package org
  :bind
  (
   :map org-mode-map
   ([remap org-narrow-to-subtree] . org-toggle-narrow-to-subtree)
   ("M-p" . org-move-subtree-up)
   ("M-n" . org-move-subtree-down)
   ("C-c o s e" . org-sort-entries)
   )
  )

(use-package org-agenda
  :bind
  ("C-c o a" . org-agenda)
  (
   :map org-mode-map
   ("C-c o m" . (lambda ()
                    (interactive)
                    ;; Filter tasks by tag
                    (org-tags-view t)))
   )
  )

(use-package org-clock
  :bind
  (
   :map org-mode-map
   ("C-c o c" . org-clock-in-last)
   ("C-c o C" . org-clock-cancel)
   )
  )

(use-package ox
  :bind
  (
   :map org-mode-map
   ("C-c o x" . org-export-dispatch)
   )
  )

(use-package org-keys
  :custom
  (org-return-follows-link t)
  (org-use-speed-commands t)
  )

(use-package ol
  :bind
  (
   :map org-mode-map
   ("C-c o l i" . org-insert-link)
   ("C-c o l s" . org-store-link)
   )
  )

(use-package org-list
  :bind
  (
   :map org-mode-map
   ("C-c o s l" . org-sort-list)
   )
  )

(use-package org-refile
  :bind
  (
   :map org-mode-map
   ("C-c o r" . org-refile)
   )
  )

(use-package org-table
  :bind
  (
   :map org-mode-map
   ("C-c o -" . org-table-insert-hline)
   )
  )

(use-package paragraphs
  :bind
  ("M-h" . mark-paragraph)
  )

(use-package profiler
  :bind
  ("<f1> T" . profiler-start)
  )

(use-package sort
  :bind
  ("C-c l d" . delete-duplicate-lines)
  )

(use-package simple
  :bind
  ("C-c c w"  . delete-trailing-whitespace)
  ("C-?" . undo-redo)
  )

(use-package casual
  :ensure t
  )

(use-package casual-agenda
  :bind
  (
   :map org-agenda-mode-map
   ("J" . bookmark-jump)
   ("M-o" . casual-agenda-tmenu)
   ("M-j" . org-agenda-clock-goto)
   )
  )

(use-package casual-bookmarks
  :bind
  (
   :map bookmark-bmenu-mode-map
   ("J" . bookmark-jump)
   ("M-o" . casual-bookmarks-tmenu)
   )
  :config
  (easy-menu-add-item global-map '(menu-bar)
                      casual-bookmarks-main-menu
                      "Tools")
  )

(use-package casual-calc
  :bind
  (
   :map calc-alg-map
   ("M-o" . casual-calc-tmenu)
   :map calc-mode-map
   ("M-o" . casual-calc-tmenu)
   )
  )

(use-package casual-dired
  :bind
  (
   :map dired-mode-map
   ("/" . casual-dired-search-replace-tmenu)
   ("s" . casual-dired-sort-by-tmenu)
   ("M-o" . casual-dired-tmenu)
   )
  :config
  (defun kv/casual-dired-context-menu-addons (menu click)
    "Customize context Menu with CLICK event."
    (easy-menu-add-item menu nil casual-dired-sort-menu)
    menu)
  :hook
  (context-menu-functions . kv/casual-dired-context-menu-addons)
  )

(use-package casual-editkit
  :bind
  ("M-o" . casual-editkit-main-tmenu)
  )

(use-package casual-ibuffer
  :bind
  (
   :map ibuffer-mode-map
   ("F" . casual-ibuffer-filter-tmenu)
   ("s" . casual-ibuffer-sortby-tmenu)
   ("M-o" . casual-ibuffer-tmenu)
   ("]" . ibuffer-forward-filter-group)
   ("[" . ibuffer-backward-filter-group)
   ("}" . ibuffer-forward-next-marked)
   ("{" . ibuffer-backwards-next-marked)
   ("$" . ibuffer-toggle-filter-group)
   )
  )

(use-package casual-info
  :bind
  (
   :map Info-mode-map
   ("B" . bookmark-set)
   ("n" . casual-info-browse-forward-paragraph)
   ("p" . casual-info-browse-backward-paragraph)
   ("M-o" . casual-info-tmenu)
   ("M-]" . Info-history-forward)
   ("M-[" . Info-history-back)
   ("l" . Info-next)
   ("h" . Info-prev)
   ("j" . Info-next-reference)
   ("k" . Info-prev-reference)
   ("/" . Info-search)
   )
  )

(use-package casual-isearch
  :bind
  (
   :map isearch-mode-map
   ("M-o" . casual-isearch-tmenu)
   )
  )

(use-package casual-re-builder
  :bind
  (
   :map reb-mode-map
   ("M-o" . casual-re-builder-tmenu)
   :map reb-lisp-mode-map
   ("M-o" . casual-re-builder-tmenu)
   )
  )

(provide 'gs-keys)
