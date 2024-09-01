;;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '(
                                 "melpa" . "https://melpa.org/packages/"
                                 ))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; 1
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package package
  :custom
  (package-install-upgrade-built-in t)
  )

(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-show-preview t)
  :config
  (auto-package-update-maybe)
  :ensure t
  )

(provide 'gs-package)
