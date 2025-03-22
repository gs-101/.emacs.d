;;; -*- lexical-binding: t -*-

(use-package elfeed
  :vc (:url "https://github.com/skeeto/elfeed")
  :bind
  (:map elfeed-search-mode-map
        ("R" . kaushal-modi/elfeed-search-mark-all-as-read))
  (:map elfeed-show-mode-map
        ("C-c C-o" . shr-browse-url))
  :config
  ;; https://emacs.stackexchange.com/a/2441
  (defun kaushal-modi/elfeed-search-mark-all-as-read ()
    "Call `mark-whole-buffer' and `elfeed-search-untag-all-undead' in unison,
marking all current messages as read."
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))
  :defer t
  :ensure t)

(use-package elfeed-org
  :vc (:url "https://github.com/remyhonig/elfeed-org")
  :after elfeed org
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files (list (expand-file-name "elfeed.org" org-directory)))
  :ensure t)

(use-package elfeed
  :bind
  (:map elfeed-show-mode-map
        ("B" . karthinks/elfeed-show-eww-open))
  (:map elfeed-search-mode-map
        ("B" . karthinks/elfeed-search-eww-open))
  :config
  (defun karthinks/elfeed-show-eww-open (&optional use-generic-p)
    "Open elfeed show entry with \\[eww]."
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-show-visit use-generic-p)))

  (defun karthinks/elfeed-search-eww-open (&optional use-generic-p)
    "Open elfeed search entry with \\[eww]."
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-search-browse-url use-generic-p))))

(provide 'gs-elfeed)
