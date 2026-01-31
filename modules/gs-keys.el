;;; -*- lexical-binding: t -*-

(use-package align
  :bind
  ("C-x |" . align-regexp))

(use-package compile
  :bind
  ("C-c c" . compile)
  ("C-c C" . recompile))

(use-package eglot
  :bind
  (:map prog-mode-map
        ("C-c t e" . eglot))
  (:map eglot-mode-map
        ("C-c e a" . eglot-code-actions)
        ("C-c e o" . eglot-code-action-organize-imports)
        ("C-c e i" . eglot-find-implementation)
        ("C-c e t" . eglot-find-typeDefinition)
        ("C-c e f" . eglot-format)
        ("C-c e r" . eglot-rename)))

(use-package elisp-mode
  :bind
  (:map emacs-lisp-mode-map
        ("C-c m C-b" . elisp-byte-compile-buffer)
        ("C-c m e d" . eval-defun)
        ("C-c m C-e" . elisp-eval-region-or-buffer)))

(use-package emacs
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-c" . gs-101/eval-dwim)
        ("C-c m e b" . eval-buffer)
        ("C-c m e r" . eval-region)
        ("C-c C-p" . ielm))
  :config
  (defun gs-101/eval-dwim (arg)
    "Evaluate region if it is active; if not, evaluate the buffer.
If the region is active, this function calls `eval-region'.
Otherwise, it calls `eval-buffer'.

If the character before point is a closed parenthesis,
this calls `eval-last-sexp'.

ARG is used for `eval-last-sexp'."
    (interactive "P")
    (cond
     ((use-region-p) (eval-region (region-beginning) (region-end) t)
      (message "Region evaluated"))
     ((eq (char-before) ?\)) (eval-last-sexp arg)
      (message "Sexp evaluated"))
     (t (eval-buffer nil nil)
        (message "Buffer evaluated")))))

(use-package files
  :bind
  (:map emacs-lisp-mode-map
        ("C-c m l" . load-library)))

(use-package find-func
  :bind
  (:map emacs-lisp-mode-map
        ("C-c m g f" . find-function)
        ("C-c m g l" . find-library)
        ("C-c m g v" . find-variable)))

(use-package flymake
  :bind
  (:map flymake-mode-map
        ([remap next-error] . flymake-goto-next-error)
        ([remap previous-error] . flymake-goto-prev-error))
  (:map project-prefix-map
        ("t f" . flymake-show-project-diagnostics))
  (:map prog-mode-map
        ("C-c t f" . flymake-start)))

(use-package dired
  :after dired
  :bind
  (:map dired-mode-map
        ("b" . dired-up-directory)))

(use-package emacs
  :bind
  ("C-c q f" . delete-frame)
  ("C-c i c" . insert-char))

(use-package eshell
  :bind
  ("C-c t s" . eshell))

(use-package files
  :bind
  ("C-c f r" . recover-this-file)
  ("C-c q r" . restart-emacs)
  ("C-c q e" . save-buffers-kill-emacs))

(use-package vc-git
  :bind
  ("M-s g v" . vc-git-grep))

(use-package grep
  :bind
  ("M-s g g" . grep)
  ("M-s g l" . lgrep)
  ("M-s g r" . rgrep))

(use-package help
  :bind
  ("C-h C-b" . describe-prefix-bindings))

(use-package ibuffer
  :bind
  ("C-x B" . ibuffer))

(use-package org
  :bind
  (:map org-mode-map
        ([remap org-narrow-to-subtree] . org-toggle-narrow-to-subtree)
        ([remap save-buffer] . org-save-all-org-buffers)
        ("M-p" . org-move-subtree-up)
        ("M-n" . org-move-subtree-down)
        ("C-c m s e" . org-sort-entries)))

(use-package org-agenda
  :bind
  ("C-c o a" . org-agenda))

(use-package org-clock
  :bind
  (:map org-mode-map
        ("C-c m c" . org-clock-in-last)
        ("C-c m C" . org-clock-cancel)))

(use-package ox
  :bind
  (:map org-mode-map
        ("C-c m x" . org-export-dispatch)))

(use-package ol
  :bind
  (:map org-mode-map
        ("C-c m l i" . org-insert-link)
        ("C-c m l s" . org-store-link)))

(use-package org-list
  :bind
  (:map org-mode-map
        ("C-c m s l" . org-sort-list)))

(use-package org-refile
  :bind
  (:map org-mode-map
        ("C-c m r" . org-refile)))

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

(use-package simple
  :bind
  ("M-g M-c" . gs-101/switch-to-minibuffer-dwim)
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
      (switch-to-minibuffer))))

(provide 'gs-keys)
