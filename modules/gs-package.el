;;; -*- lexical-binding: t -*-

(mapc (lambda (archive)
        (add-to-list 'package-archives archive)) '(
        ("melpa" . "https://melpa.org/packages/")
        ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
        ))

(provide 'gs-package)
