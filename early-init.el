;;; -*- lexical-binding: t -*-

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

(defun gs-101/filename (name &optional default-directory)
  "Return a path to FILE suitable for the current operating system.
DEFAULT-DIRECTORY defines where to start looking for the file."
  (if default-directory
      (convert-standard-filename (expand-file-name name default-directory))
    (convert-standard-filename (expand-file-name name))))

(defcustom gs-101/projects-code-directory (gs-101/filename "~/Projects/code/")
  "Path for project related to code, like applications."
  :type 'directory)

(defcustom gs-101/modules-directory (gs-101/filename "modules" user-emacs-directory)
  "Path for this configuration's modules."
  :type 'directory)

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
  (gs-101/add-many-to-list
   'package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))))
