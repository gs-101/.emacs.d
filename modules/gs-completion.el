;;; -*- lexical-binding: t -*-

(use-package cape
  :vc (:url "https://github.com/minad/cape")
  :ensure t
  :custom
  (cape-file-prefix "/")
  :hook
  (completion-at-point-functions . cape-dabbrev)
  (completion-at-point-functions . cape-file))

(use-package tempel
  :vc (:url "https://github.com/minad/tempel")
  :ensure t
  :hook
  (completion-at-point-functions . tempel-complete))

(use-package lsp-snippet
  :after tempel eglot
  :vc (:url "https://github.com/svaante/lsp-snippet")
  :config
  (lsp-snippet-tempel-eglot-init))

(use-package tempel-snippets
  :vc (:url "https://github.com/gs-101/tempel-snippets")
  :after tempel
  :ensure t)

(use-package vertico
  :custom
  (vertico-cycle t)
  :ensure t
  :init
  (vertico-mode)
  (vertico-multiform-mode))

(use-package vertico-directory
  :after vertico
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :after vertico
  :config
  :custom
  (vertico-multiform-categories
   '((symbol (vertico-sort-function . vertico-sort-alpha))
     (file (vertico-sort-function . vertico-sort-directories-first)))))

(use-package marginalia
  :vc (:url "https://github.com/minad/marginalia")
  :ensure t
  :init
  (marginalia-mode))

(provide 'gs-completion)
