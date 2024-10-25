;;; -*- lexical-binding: t -*-

(use-package citar
  :custom
  (citar-bibliography org-cite-global-bibliography)
  (citar-library-paths '("~/Documents/Books/"))
  (citar-open-entry-function 'citar-open-entry-in-zotero)
  :hook
  (org-mode . citar-capf-setup)
  :ensure t
  )

(use-package citar-embark
  :after citar embark
  :config
  (citar-embark-mode)
  :custom
  (citar-at-point-function 'embark-act)
  :ensure t
  )

(use-package citar-embark
  :after citar embark
  :config
  (defun bibtex-key-target-finder ()
    "Embark target for bibliography files."
    (save-excursion
      (bibtex-beginning-of-entry)
      (when (looking-at bibtex-entry-maybe-empty-head)
        (cons 'bibtex-key (bibtex-key-in-head)))))
  (add-to-list 'embark-target-finders 'bibtex-key-target-finder)
  (defvar citar-embark-bibtex-map
    "Embark keymap for BibTeX entries."
    (let ((map (make-sparse-keymap)))
      (define-key map "o" #'citar-open)
      (define-key map "n" #'citar-open-notes)
      map))
  (add-to-list 'embark-keymap-alist '(bibtex-key . citar-embark-bibtex-map))
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

(use-package citar-embark
  :after citar embark
  :config
  (defun ex/search-pdf-contents (keys-entries &optional str)
    "Search the contents of PDFs."
    (interactive (list (citar-select-refs)))
    (let ((files (citar-file--files-for-multiple-entries
                  (citar--ensure-entries keys-entries)
                  citar-library-paths
                  '("pdf")))
          (search-str (or str (read-string "Search string: "))))
      (pdf-occur-search files search-str t)))

  ;; with this, you can exploit embark's multitarget actions, so that you can run `embark-act-all`
  (add-to-list 'embark-multitarget-actions #'ex/search-pdf-contents)
  )

(use-package oc
  :bind
  (
   :map org-mode-map
   ("C-c m q" . org-cite-insert)
   )
  :custom
  (org-cite-csl-styles-dir (expand-file-name "~/Documents/Zotero/styles/"))
  (org-cite-export-processors '(t biblatex))
  (org-cite-global-bibliography '("~/Documents/Bibliography.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  )

(use-package citar-org-roam
  :requires org-roam
  :after citar
  :config
  (citar-org-roam-mode)
  :custom
  (citar-org-roam-note-title-template "${title}")
  :ensure t
  )

(provide 'gs-citar)
