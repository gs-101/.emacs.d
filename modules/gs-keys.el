;;; -*- lexical-binding: t -*-

(use-package align
  :bind
  ("C-x |" . align-regexp)
  )

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
   ("C-c t e" . eglot)
   :map eglot-mode-map
   ("C-c e a" . eglot-code-actions)
   ("C-c e o" . eglot-code-action-organize-imports)
   ("C-c e i" . eglot-find-implementation)
   ("C-c e t" . eglot-find-typeDefinition)
   ("C-c e f" . eglot-format)
   ("C-c e r" . eglot-rename)
   )
  )

(use-package elisp-mode
  :bind
  (
   :map emacs-lisp-mode-map
        ("C-c m C-b" . elisp-byte-compile-buffer)
        ("C-c m e d" . eval-defun)
        ("C-c m C-e" . elisp-eval-region-or-buffer)
        )
  )

(use-package emacs
  :bind
  (
   :map emacs-lisp-mode-map
   ("C-c C-c" . gs-101/eval-dwim)
   ("C-c m e b" . eval-buffer)
   ("C-c m e r" . eval-region)
   ("C-c C-p" . ielm)
   )
  :config
  (defun gs-101/eval-dwim ()
    "Evaluate region if it is active; if not, evaluate the buffer.
If the region is active, this function calls `eval-region'.
Otherwise, it calls `eval-buffer'."
    (interactive)
    (if (use-region-p)
        (eval-region (region-beginning) (region-end) t)
      (eval-buffer nil nil)))
  )

(use-package files
  :bind
  (
   :map  emacs-lisp-mode-map
         ("C-c m l" . load-library)
         )
  )

(use-package find-func
  :bind
  (
   :map emacs-lisp-mode-map
   ("C-c m g f" . find-function)
   ("C-c m g l" . find-library)
   ("C-c m g v" . find-variable)
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

(use-package emacs
  :bind
  ("C-c q f" . delete-frame)
  ("C-c i c" . insert-char)
  )

(use-package emoji
  :bind
  ("C-c i e" . emoji-insert)
  )

(use-package files
  :bind
  ("C-c f r" . recover-this-file)
  ("C-c b r" . revert-buffer)
  ("C-c q r" . restart-emacs)
  ("C-c q K" . save-buffers-kill-emacs)
  )

(use-package find-func
  :bind
  ("<f1> P" . find-library)
  )

(use-package vc-git
  :bind
  ("M-s g v" . vc-git-grep)
  )

(use-package grep
  :bind
  ("M-s g g" . grep)
  ("M-s g l" . lgrep)
  ("M-s g r" . rgrep)
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

(use-package org
  :bind
  (
   :map org-mode-map
   ([remap down-list] . org-next-visible-heading)
   ([remap backward-up-list] . org-previous-visible-heading)
   ([remap org-narrow-to-subtree] . org-toggle-narrow-to-subtree)
   ([remap save-buffer] . org-save-all-org-buffers)
   ("M-p" . org-move-subtree-up)
   ("M-n" . org-move-subtree-down)
   ("C-c m s e" . org-sort-entries)
   )
  )

(use-package org-agenda
  :bind
  ("C-c o a" . org-agenda)
  (
   :map org-mode-map
   ("C-c m m" . (lambda ()
                    (interactive)
                    ;; Filter tasks by tag
                    (org-tags-view t)))
   )
  )

(use-package org-clock
  :bind
  (
   :map org-mode-map
   ("C-c m c" . org-clock-in-last)
   ("C-c m C" . org-clock-cancel)
   )
  )

(use-package ox
  :bind
  (
   :map org-mode-map
   ("C-c m x" . org-export-dispatch)
   )
  )

(use-package org-keys
  :custom
  (org-use-speed-commands t)
  )

(use-package ol
  :bind
  (
   :map org-mode-map
   ("C-c m l i" . org-insert-link)
   ("C-c m l s" . org-store-link)
   )
  )

(use-package org-list
  :bind
  (
   :map org-mode-map
   ("C-c m s l" . org-sort-list)
   )
  )

(use-package org-refile
  :bind
  (
   :map org-mode-map
   ("C-c m r" . org-refile)
   )
  )

(use-package org-table
  :bind
  (
   :map org-mode-map
   ("C-c m -" . org-table-insert-hline)
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

(use-package repeat
  :init
  (repeat-mode)
  )

(use-package sort
  :bind
  ("C-c l d" . delete-duplicate-lines)
  )

(use-package simple
  :bind
  ("C-c c w"  . delete-trailing-whitespace)
  ("M-g M-c" . gs-101/switch-to-minibuffer-dwim)
  ("C-?" . undo-redo)
  ("M-\\" . nil) ;; unbind `delete-horizontal-space', use `cycle-spacing' instead
  ([remap capitalize-word] . capitalize-dwim)
  ([remap upcase-word] . upcase-dwim)
  ([remap downcase-word] . downcase-dwim)
  :config
  (defun gs-101/switch-to-minibuffer-dwim ()
    "Switch to minibuffer in a regular window. In minibuffer, switch to previous window.
If currently in the minibuffer, this function calls `previous-window-any-frame'.
Otherwise, it calls `switch-to-minibuffer'."
    (interactive)
    (if (minibufferp)
        (previous-window-any-frame)
      (switch-to-minibuffer)))
  )

(use-package shell
  :bind
  ("C-c t s" . shell)
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

(use-package casual-calendar
  :bind
  (
   :map calendar-mode-map
   ("M-o" . casual-calendar)
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
