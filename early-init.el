;;; -*- lexical-binding: t -*-

;; 1
(if (and (featurep #'native-compile)
         (fboundp #'native-comp-available-p)
         (native-comp-available-p))
    (setq native-comp-jit-compilation t
          package-native-compile t)
  ;; 2
  (setq features (delq #'native-compile features)))

;; Disable package initialization on startup
;; packages are only used in the modules.
(setq package-enable-at-startup nil)

(when (fboundp #'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(defcustom gs-101/projects-code-directory (convert-standard-filename (expand-file-name "~/Projects/Code/"))
  "Path for project related to code, like applications."
  :type 'directory
  )
(defcustom gs-101/modules-directory (convert-standard-filename (expand-file-name "modules" user-emacs-directory))
  "Path for this configuration's modules."
  :type 'directory
  )
