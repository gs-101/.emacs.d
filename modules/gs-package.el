;;; -*- lexical-binding: t -*-

(require 'package)
(package-initialize)
(add-to-list 'package-archives '(
                                 "melpa" . "https://melpa.org/packages/"
                                 ))

;; 1
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(provide 'gs-package)
