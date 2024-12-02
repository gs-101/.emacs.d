;;; -*- lexical-binding: t -*-

(require 'package)
(package-initialize)
(mapc (lambda (archive)
        (add-to-list 'package-archives archive)) '(
        ("melpa" . "https://melpa.org/packages/")
        ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
        ))

;; 1
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(provide 'gs-package)
