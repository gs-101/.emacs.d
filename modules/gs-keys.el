;;; -*- lexical-binding: t -*-

(use-package eglot
  :bind
  (
   :map prog-mode-map
   ("C-c C-x e" . eglot)
   )
  )

(use-package flymake
  :bind
  (
   :map prog-mode-map
   ("C-c C-x f" . flymake-start) 
   )
  )

(use-package emacs
  :bind
  ("C-c i c" . insert-char)
  ("C-c k e" . kill-emacs)
  )

(use-package emoji
  :bind
  ("C-c i e" . emoji-insert)
  )

(use-package face-remap
  :bind
  ("C-=" . text-scale-increase)
  ("C--" . text-scale-decrease)
  )

(use-package files
  :bind
  ("C-c f r" . recover-this-file)
  ("C-c b r" . revert-buffer)
  )

(use-package grep
  :bind
  ("C-c g g" . grep)
  ("C-c g l" . lgrep)
  ("C-c g r" . rgrep)
  )

(use-package help
  :bind
  ("C-h C-b" . describe-prefix-bindings)
  )

(use-package isearch
  :bind
  ("C-s" . isearch-forward-word)
  :custom
  (lazy-highlight-initial-delay 0.0)
  )

(use-package lisp
  :bind
  ("C-c p c" . check-parens)
  :defer t
  :demand t
  :ensure nil
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
   ("C-c o v q" . (lambda ()
                    (interactive)
                    ;; Filter tasks by tag
                    (org-tags-view t)))
   )
  )

(use-package org-capture
  :bind
  (
   :map org-mode-map
   ("C-c o c" . org-capture)
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

(use-package sort
  :bind
  ("C-c l d" . delete-duplicate-lines)
  )

(use-package casual-agenda
  :bind
  (
   :map org-agenda-mode-map
   ("M-o" . casual-agenda-tmenu)
   )
  :ensure t
  )

(use-package casual-bookmarks
  :bind
  (
   :map bookmark-bmenu-mode-map
   ("J" . bookmark-jump)
   ("M-o" . casual-bookmarks-tmenu)
   ("S" . casual-bookmarks-sortby-tmenu)
   )
  :config
  (easy-menu-add-item global-map '(menu-bar)
                      casual-bookmarks-main-menu
                      "Tools")
  :ensure t
  )

(use-package casual-calc
  :bind
  (
   :map calc-mode-map
   ("M-o" . casual-calc-tmenu)
   :map calc-alg-map
   ("M-o" . casual-calc-tmenu)
   )
  :ensure t
  )

(use-package casual-dired
  :bind
  (
   :map dired-mode-map
   ("M-o" . casual-dired-tmenu)
   )
  :ensure t
  )

(use-package casual-ibuffer
  :bind
  (
   :map ibuffer-mode-map
   ("M-o" . casual-ibuffer-tmenu)
   ("F" . casual-ibuffer-filter-tmenu)
   ("s" . casual-ibuffer-sortby-tmenu)
   ("<double-mouse-1>" . ibuffer-visit-buffer)
   ("M-<double-mouse-1>" . ibuffer-visit-buffer-other-window)
   ("{" . ibuffer-backwards-next-marked)
   ("}" . ibuffer-forward-next-marked)
   ("[" . ibuffer-backward-filter-group)
   ("]" . ibuffer-forward-filter-group)
   ("$" . ibuffer-toggle-filter-group)
   )
  :ensure t
  )

(use-package casual-info
  :bind
  (
   :map Info-mode-map
   ("M-o" . casual-info-tmenu)
   ("M-[" . Info-history-back)
   ("M-]" . Info-history-forward)
   ("p" . casual-info-browse-backward-paragraph)
   ("n" . casual-info-browse-forward-paragraph)
   ("B" . bookmark-set)
   )
  :ensure t
  :hook
  (info-mode . scroll-lock-mode)
  )

(use-package casual-isearch
  :bind
  (
   :map isearch-mode-map
   ("M-o" . casual-isearch-tmenu)
   )
  :ensure t
  )

(use-package casual-re-builder
  :bind
  (
   :map reb-mode-map
   ("M-o" . casual-re-builder-tmenu)
   :map reb-lisp-mode-map
   ("M-o" . casual-re-builder-tmenu)
   )
  :ensure t
  )

(provide 'gs-keys)
