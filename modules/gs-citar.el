;;; -*- lexical-binding: t -*-

(use-package citar
  :vc (:url "https://github.com/emacs-citar/citar")
  :custom
  (citar-bibliography "~/Documents/bibliography.bib")
  (citar-citeproc-csl-styles-dir "~/Documents/zotero/styles/")
  (citar-citeproc-csl-style "harvard-cite-them-right.csl")
  (citar-format-reference-function #'citar-citeproc-format-reference)
  (citar-library-paths '("~/Documents/zotero/storage/"))
  (citar-open-entry-function #'citar-open-entry-in-zotero)
  (citar-templates '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}") (suffix . "          ${=key= id:15}    ${tags keywords keywords:*}    ${abstract abstract:*}") (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.
") (note . "Notes on ${author editor:%etal}, ${title}")))
  :hook
  (org-mode . citar-capf-setup)
  :ensure t
  )

(use-package citar-embark
  :after embark
  :custom
  (citar-at-point-function #'embark-act)
  :hook
  (text-mode . citar-embark-mode)
  )

(use-package citar-embark
  :after citar-embark
  :config
  (setf (alist-get
         'key-at-point
         (alist-get '(org-mode) citar-major-mode-functions nil nil #'equal))
        #'bdarcus/citar-org-key-at-point)

  (defun bdarcus/citar-org-key-at-point ()
    "Return citekey at point, when in org property drawer.

Citekey must be formatted as `@key'."
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
  (org-cite-global-bibliography '("~/Documents/bibliography.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  )

(use-package oc-csl
  :after oc
  :custom
  (org-cite-csl-styles-dir "~/Documents/zotero/styles/")
  (org-cite-export-processors '((t . (csl "harvard-cite-them-right.csl"))))
  )

(use-package citar-org-roam
  :vc (:url "https://github.com/emacs-citar/citar-org-roam")
  :bind
  ("C-z r c" . citar-create-note)
  :after org-roam
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
