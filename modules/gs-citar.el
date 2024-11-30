;;; -*- lexical-binding: t -*-

(use-package citar
  :custom
  (citar-bibliography "~/Documents/Bibliography.bib")
  (citar-citeproc-csl-styles-dir "~/Documents/Zotero/styles/")
  (citar-citeproc-csl-style "harvard-cite-them-right.csl")
  (citar-format-reference-function 'citar-citeproc-format-reference)
  (citar-library-paths '("~/Documents/Zotero/storage/"))
  (citar-open-entry-function 'citar-open-entry-in-zotero)
  :hook
  (org-mode . citar-capf-setup)
  :ensure t
  )

(use-package citar-embark
  :after citar embark
  :custom
  (citar-at-point-function 'embark-act)
  :ensure t
  :hook
  (text-mode . citar-embark-mode)
  )

(use-package citar-embark
  :after citar embark
  :config
  (setf (alist-get
         'key-at-point
         (alist-get '(org-mode) citar-major-mode-functions nil nil #'equal))
        #'my/citar-org-key-at-point)

  (defun my/citar-org-key-at-point ()
    "Return citekey at point, when in org property drawer.

Citkey must be formatted as `@key'."
    (or (citar-org-key-at-point)
        (when (and (equal (org-element-type (org-element-at-point)) 'node-property)
                   (org-in-regexp (concat "[[:space:]]" org-element-citation-key-re)))
          (cons (substring (match-string 0) 2)
                (cons (match-beginning 0)
                      (match-end 0))))))
  (add-to-list 'embark-keymap-alist '(bib-reference . citar-map))
  )

(use-package oc
  :bind
  (
   :map org-mode-map
   ("C-c m q" . org-cite-insert)
   )
  :custom
  (org-cite-csl-styles-dir "~/Documents/Zotero/styles/")
  (org-cite-export-processors '((t . (csl "harvard-cite-them-right.csl"))))
  (org-cite-global-bibliography '("~/Documents/Bibliography.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  )

(use-package citar-org-roam
  :bind
  ("C-z r c" . citar-create-note)
  :after citar org-roam
  :config
  (citar-org-roam-mode)
  (add-to-list 'org-roam-capture-templates
               '("b" "bibliographic" plain
                (file "~/Documents/Org Roam/Templates/default.org")
                :if-new
                (file+head "%<%Y%m%d%H%M%S>-${citar-citekey}.org" "#+title: ${title}\n\n")
                :unnarrowed t))
  :custom
  (citar-org-roam-capture-template-key "b")
  (citar-org-roam-note-title-template "${title}")
  :ensure t
  )

(provide 'gs-citar)
