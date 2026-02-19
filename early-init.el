;;; -*- lexical-binding: t -*-

(defmacro gs-101/run-at-time-daily (time &rest body)
  "Runs BODY daily at TIME."
  `(run-at-time ,time (* 60 60 24)
                (lambda ()
                  ,@body)))

(defcustom gs-101/projects-code-directory (expand-file-name "~/Projects/code/")
  "Path for project related to code, like applications."
  :type 'directory)

(defcustom gs-101/modules-directory (expand-file-name "modules" user-emacs-directory)
  "Path for this configuration's modules."
  :type 'directory)

(defcustom dw/current-distro
  (or (and (eq system-type 'gnu/linux)
           (file-exists-p "/etc/os-release")
           (with-temp-buffer
             (insert-file-contents "/etc/os-release")
             (search-forward-regexp "^ID=\"?\\(.*\\)\"?$")
             (intern (or (match-string 1)
                         "unknown"))))
      'unknown)
  "Current GNU/Linux distro being used."
  :type 'symbol)

(defvar dw/guix-p
  (eql dw/current-distro 'guix))

(defvar gs-101/nobara-p
  (eql dw/current-distro 'nobara))

(defun gs-101/fizz-buzz (num)
  "Play the FizzBuzz game from 1 to NUM.
This was written from a distraction while reading this:
<https://www.theodinproject.com/lessons/foundations-problem-solving#solving-fizz-buzz>

This is my first time using `cl-loop' for anything."
  (interactive "n")
  (cl-loop for n from 1 to num
           if (and (eq (% n 3) 0) (eq (% n 5) 0))
           collect "Fizzbuzz" into output
           if (eq (% n 3) 0)
           collect "Fizz" into output
           if (eq (% n 5) 0)
           collect "Buzz" into output
           else
           collect n into output
           finally return (print output)))

(with-eval-after-load 'package
  (dolist (archive '(("melpa" . "https://melpa.org/packages/")
                     ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))
    (add-to-list 'package-archives archive)))
