;;; -*- lexical-binding: t -*-

(use-package meow
  :ensure t
  )

(use-package meow-core
  :init
  (meow-global-mode)
  )

(use-package meow-var
  :config
  (add-to-list 'meow-char-thing-table '(?a . arrow))
  :custom
  (meow-use-clipboard t)
  )

(use-package meow-cheatsheet
  :custom
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  )

(use-package meow-cheatsheet-layout
  :custom
  (meow-cheatsheet-layout-qwerty t)
  )

(use-package meow-helpers
  :config
  (meow-leader-define-key
   '("z" . "C-z")
   ;; Use SPC (0-9) for digit arguments.
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   )
  (meow-motion-overwrite-define-key
   '("<escape>" . ignore)
   )
  )

(use-package meow-helpers
  :config
  (meow-normal-define-key
   '("-" . negative-argument)
   '("+" . meow-universal-argument)
   '("," . meow-pop-marker)
   '("." . meow-find-ref)
   '("[" . meow-inner-of-thing)
   '("]" . meow-bounds-of-thing)
   '("'" . repeat)
   '("=" . meow-indent)
   '("|" . align-regexp)
   '("<escape>" . ignore)
   '("?" . meow-comment)
   '("%" . meow-query-replace)
   '("&" . meow-query-replace-regexp)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("0" . meow-expand-0)
   '("a" . meow-back-to-indentation)
   '("A" . meow-beginning-of-thing)
   '("b" . meow-left)
   '("B" . backward-sexp)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("e" . move-end-of-line)
   '("E" . meow-end-of-thing)
   '("f" . meow-right)
   '("F" . forward-sexp)
   '("g" . meow-grab)
   '("G" . meow-swap-grab)
   '("h" . meow-block)
   '("H" . mark-sexp)
   '("i" . meow-insert)
   '("I" . meow-append)
   '("j" . meow-pop-to-mark)
   '("J" . meow-pop-to-global-mark)
   '("k" . meow-kill)
   '("K" . meow-kill-whole-line)
   '("l" . meow-visual-line)
   '("L" . meow-visual-line-expand)
   '("m" . meow-mark-word)
   '("n" . meow-next)
   '("N" . down-list)
   '("o" . meow-next-word)
   '("O" . meow-back-word)
   '("p" . meow-prev)
   '("P" . backward-up-list)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-query-replace-regexp)
   '("s" . isearch-forward)
   '("t" . meow-transpose-sexp)
   '("u" . undo)
   '("U" . undo-redo)
   '("v" . meow-visit)
   '("w" . meow-save)
   '("y" . meow-yank)
   '("Y" . meow-yank-pop)
   '("z" . meow-till)
   '("Z" . meow-till-expand)
   )
  )

(use-package meow-thing
  :config
  (meow-thing-register 'arrow '(pair ("<") (">")) '(pair ("<") (">")))
  )

(use-package meow-command
  :after meow avy
  :bind
  (
   :map meow-normal-state-keymap
   ([remap meow-pop-to-mark] . favetelinguis/meow-jumper)
   :map meow-word-state-keymap
   ([remap meow-pop-to-mark] . favetelinguis/meow-jumper)
   )
  :config
  (defun gs-101/meow-avy-action-kill-whole-line (pt)
    "Jump to target at marker PT, killing its whole line after the jump.
This follows the parameters set by `meow-kill-whole-line'."
    (save-excursion
      (goto-char pt)
      (meow-kill-whole-line)))

  (defun gs-101/meow-avy-action-block (pt)
    "Mark block at PT."
    (goto-char pt)
    (meow-block pt))

  (setf
   (alist-get ?K avy-dispatch-alist) 'gs-101/meow-avy-action-kill-whole-line
   (alist-get ?m avy-dispatch-alist) 'gs-101/meow-avy-action-block
   )

  (defun favetelinguis/meow-jumper (&optional arg)
    "Switch between Meow search and Avy,
depending on if the region is active.
If the region is active, this function calls `meow-search'.
Otherwise, it calls `avy-goto-char-timer."
    (interactive)
    (if (region-active-p)
        (meow-search arg)
      (avy-goto-char-timer)))
  )

(use-package meow-tree-sitter
  :ensure t
  :config
  (meow-tree-sitter-register-defaults)
  )

(use-package meow-helpers
  :after embark
  :config
  (meow-normal-define-key
   '(";" . embark-act))
  )

(use-package meow-core
  :after org-appear
  :custom
  (org-appear-trigger 'manual)
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (add-hook 'meow-insert-enter-hook
                                       #'org-appear-manual-start
                                       nil
                                       t)
                             (add-hook 'meow-insert-exit-hook
                                       #'org-appear-manual-stop
                                       nil
                                       t)))
  )

(provide 'gs-keys-meow)
