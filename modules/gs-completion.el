;;; -*- lexical-binding: t -*-

(use-package orderless
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(
                                 orderless-annotation
                                 orderless-initialism
                                 orderless-literal-prefix
                                 orderless-regexp
                                 )))
  :custom
  (completion-category-defaults nil)
  (completion-category-overrides '(
                                   (file (styles partial-completion))
                                   (minibuffer (initials orderless))
                                   ))
  (completion-styles '(
                       orderless
                       ))
  (orderless-matching-styles '(
                               orderless-literal
                               orderless-regexp
                               ))
  (orderless-style-dispatchers (list
                                #'orderless-affix-dispatch
                                #'+orderless-consult-dispatch
                                ))
  :ensure t
  :preface
  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))
  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))))
  )

(use-package cape
  :ensure t
  :hook
  (completion-at-point-functions . cape-dabbrev)
  (completion-at-point-functions . cape-file)
  )

(use-package corg
  :vc (:url "https://github.com/isamert/corg.el")
  :ensure t
  :hook
  (org-mode . corg-setup)
  )

(use-package tempel
  :bind
  ("C-c i s" . tempel-insert)
  :custom
  (tempel-trigger-prefix "<")
  :ensure t
  :hook
  (completion-at-point-functions . tempel-complete)
  (prog-mode . tempel-abbrev-mode)
  )

(use-package eglot-tempel
  :requires tempel
  :after eglot
  :ensure t
  :config
  (eglot-tempel-mode)
  )

(use-package tempel-collection
  :requires tempel
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
  (corfu-auto t)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preselect 'directory)
  :ensure t
  :init
  (global-corfu-mode)
  )

(use-package eldoc
  :custom
  (global-eldoc-mode nil)
  )

(use-package orderless
  :requires (corfu orderless)
  :custom
  (completion-category-overrides '((eglot (styles orderless))
                                   (eglot-capf (styles orderless))))
  )

(use-package cape
  :requires (corfu cape)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  )

(use-package emacs
  :requires (corfu cape tempel)
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
  (eglot-managed-mode-hook . minad/eglot-capf)
  )

(use-package vertico
  :custom
  (vertico-cycle t)
  :ensure t
  :init
  (vertico-mode)
  :preface
  (defun crm-indicator (args)
    "Add a prompt indicator to `completing-read-multiple'."
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (aref args))
          (aref args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  )

(use-package vertico-directory
  :requires vertico
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

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  )

(provide 'gs-completion)
