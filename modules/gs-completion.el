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
                                   ))
  (completion-styles '(orderless))
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
  :config
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  :ensure t
  )

(use-package corg
  :vc (:url "https://github.com/isamert/corg.el")
  :config
  (add-hook 'org-mode-hook #'corg-setup)
  :ensure t
  )

(use-package tempel
  :config
  (add-hook 'completion-at-point-functions #'tempel-complete)
  :custom
  (tempel-trigger-prefix "<")
  :ensure t
  :hook
  (prog-mode . tempel-abbrev-mode)
  )

(use-package eglot-tempel
  :ensure t
  :hook
  (eglot . eglot-tempel-mode)
  )

(use-package tempel-collection
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
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-cycle t)
  (corfu-preselect 'directory)
  :ensure t
  )

(use-package orderless
  :custom
  (completion-category-overrides '((eglot (styles orderless))
                                   (eglot-capf (styles orderless))))
  )

(use-package cape
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  )

(use-package corfu
  :if (package-installed-p '(cape tempel))
  :after (cape tempel)
  :preface
  (defun minad/eglot-capf ()
    "eglot capf with tempel and cape features."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'cape-file
                       #'eglot-completion-at-point
                       #'tempel-expand
                       ))))
  (add-hook 'eglot-managed-mode-hook #'minad/eglot-capf)
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

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  )

(provide 'gs-completion)
