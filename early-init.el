;;; -*- lexical-binding: t -*-

;; 1
(if (and (featurep #'native-compile)
         (fboundp #'native-comp-available-p)
         (native-comp-available-p))
    (setq native-comp-jit-compilation t
          package-native-compile t)
  ;; 2
  (setq features (delq #'native-compile features)))

(when (fboundp #'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(defcustom gs-101/projects-code-directory (convert-standard-filename (expand-file-name "~/Projects/code/"))
  "Path for project related to code, like applications."
  :type 'directory)

(defcustom gs-101/modules-directory (convert-standard-filename (expand-file-name "modules" user-emacs-directory))
  "Path for this configuration's modules."
  :type 'directory)

(defun gs-101/guix-p ()
  "Check if guix is installed in the current system."
  (executable-find "guix"))

(defun gs-101/nobara-p ()
  "Check if the current system uses Nobara Linux as its distribution."
  (executable-find "nobara-welcome"))

(defun gs-101/add-many-to-list (list many)
  "Return a function for adding MANY items to a LIST.
Without putting them in a separate list."
  (mapc (lambda (item)
          (add-to-list list item)) many))

(with-eval-after-load 'package
  (gs-101/add-many-to-list
   'package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))))
