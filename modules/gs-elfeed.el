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

(use-package elfeed-protocol
  :vc (:url "https://github.com/fasheng/elfeed-protocol")
  :ensure t
  :after elfeed
  :config
  (elfeed-protocol-enable)
  :custom
  (elfeed-feeds
   '(("fever+https://gabriel@nix-pc.tailbf3a7f.ts.net"
      :api-url "https://nix-pc.tailbf3a7f.ts.net/fever/"
      :password (auth-source-pick-first-password :host "nix-pc.tailbf3a7f.ts.net"))))
  (elfeed-protocol-fever-fetch-category-as-tag t)
  (elfeed-protocol-fever-update-unread-only nil)
  (elfeed-use-curl t))

(provide 'gs-elfeed)
