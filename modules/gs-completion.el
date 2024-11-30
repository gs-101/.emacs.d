;;; -*- lexical-binding: t -*-

(use-package orderless
  :config
  (orderless-define-completion-style minad/orderless-initialism
    (orderless-matching-styles '(
                                 orderless-initialism
                                 orderless-literal
                                 orderless-regexp
                                 )))
  (orderless-define-completion-style minad/orderless-simple
    (orderless-style-dispatchers nil)
    (orderless-matching-styles '(orderless-literal)))
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '(
                                   (file (styles partial-completion))
                                   (command (styles minad/orderless-initialism))
                                   (variable (styles minad/orderless-initialism))
                                   (symbol (styles minad/orderless-initialism))
                                   (minibuffer (styles minad/orderless-initialism))
                                   ))
  (orderless-comment-separator #'orderless-escapable-split-on-space)
  (orderless-style-dispatchers (list
                                #'minad/orderless-consult-dispatch
                                #'orderless-affix-dispatch
                                ))
  :ensure t
  :preface
  (defun minad/orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))
  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun minad/orderless-consult-dispatch (word _index _total)
    "Ensure that $ works with Consult commands, witch add disambiguation suffixes."
    (cond
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (minad/orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (minad/orderless--consult-suffix))))))
  )

(use-package cape
  :ensure t
  :hook
  (completion-at-point-functions . cape-dabbrev)
  (completion-at-point-functions . cape-file)
  )

(use-package cape
  :config
  (defun minad/emacs-lisp-ignore-keywords (cand)
    "Remove keywords from the CAND list, unless the completion text starts with a `:'."
    (or (not (keywordp cand))
        (eq (char-after (car completion-in-region--data)) ?:)))
  (defun minad/emacs-lisp-capf ()
    "`completion-at-point-functions' for `emacs-lisp-mode', including support for symbols currently unknown to Emacs, using `cape-dabbrev'.
Also adds `cape-file' as a fallback."
    (setq-local completion-at-point-functions
                `(,(cape-capf-super
                    (cape-capf-predicate
                     #'elisp-completion-at-point
                     #'minad/emacs-lisp-ignore-keywords)
                    #'cape-dabbrev)
                  cape-file)
                cape-dabbrev-min-length 5))
  :hook
  (emacs-lisp-mode . minad/emacs-lisp-capf)
  )

(use-package corg
  :vc (:url "https://github.com/isamert/corg.el")
  :ensure t
  :hook
  (org-mode . corg-setup)
  )

(use-package tempel
  :bind
  ("C-z i s" . tempel-insert)
  :ensure t
  )

(use-package eglot-tempel
  :after eglot tempel
  :ensure t
  :config
  (eglot-tempel-mode)
  )

(use-package tempel-snippets
  :vc (:url "https://github.com/gs-101/tempel-snippets")
  :after tempel
  :ensure t
  )

(use-package corfu
  :bind
  (
   :map corfu-map
   ("M-SPC" . corfu-insert-separator)
   ("RET" . nil)
   )
  :config
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto nil)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preselect 'directory)
  :ensure t
  :init
  (global-corfu-mode)
  )

(use-package corfu
  :after orderless
  :hook
  (corfu-mode . (lambda () (setq-local completion-styles '(minad/orderless-simple))))
  )

(use-package minibuffer
  :after eglot
  :custom
  (completion-category-defaults nil)
  )

(use-package cape
  :after corfu cape
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  )

(use-package emacs
  :after corfu cape tempel
  :preface
  (defun minad/eglot-capf ()
    "eglot capf with tempel and cape features."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'cape-file
                       #'eglot-completion-at-point
                       #'tempel-expand
                       ))))
  :hook
  (eglot-managed-mode . minad/eglot-capf)
  )

(use-package vertico
  :custom
  (vertico-cycle t)
  :ensure t
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  )

(use-package vertico-directory
  :after vertico
  :bind
  (
   :map vertico-map
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word)
   )
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  )

(use-package vertico-multiform
  :after vertico
  :config
  (defun minad/sort-directories-first (files)
    "Sort FILES by directories first, but still maintain the history, length and alphabetical sorting.
Hidden directories have a higher priority."
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
  :custom
  (vertico-multiform-categories '(
                                  (symbol (vertico-sort-function . vertico-sort-alpha))
                                  (file (vertico-sort-function . minad/sort-directories-first))
                                  ))
  )

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  )

(provide 'gs-completion)
