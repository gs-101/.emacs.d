;;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '(
                                 ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
                                 ("melpa" . "https://melpa.org/packages/")
                                 ))
(setq package-archives-priorities '(
                                    ("jcs-elpa" . 0)
                                    ("melpa" . 5)
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
