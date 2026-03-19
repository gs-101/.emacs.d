;;; -*- lexical-binding: t -*-

(use-package align
  :bind
  ("C-x |" . align-regexp))

(use-package flymake
  :bind
  (:map flymake-mode-map
        ([remap next-error] . flymake-goto-next-error)
        ([remap previous-error] . flymake-goto-prev-error))
  (:map project-prefix-map
        ("t f" . flymake-show-project-diagnostics))
  (:map prog-mode-map
        ("C-c t f" . flymake-start)))

(use-package vc-git
  :bind
  ("M-s g v" . vc-git-grep))

(use-package help
  :bind
  ("C-h C-b" . describe-prefix-bindings))

(use-package ibuffer
  :bind
  ("C-x B" . ibuffer))

(use-package ol
  :bind
  (:map org-mode-map
        ("C-c m l i" . org-insert-link)
        ("C-c m l s" . org-store-link)))

(use-package org-table
  :bind
  (:map org-mode-map
        ("C-c m -" . org-table-insert-hline)))

(use-package recentf
  :bind
  ("M-g r" . recentf))

(use-package repeat
  :init
  (repeat-mode))

(use-package sort
  :bind
  ("C-c l d" . delete-duplicate-lines))

(provide 'gs-keys)
