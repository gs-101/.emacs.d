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

(provide 'gs-package)
