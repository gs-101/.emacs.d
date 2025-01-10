;;; -*- lexical-binding: t -*-

(use-package meow
  :vc (:url "https://github.com/meow-edit/meow")
  :ensure t
  )

(use-package meow-core
  :init
  (meow-global-mode)
  )

(use-package meow-var
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

(use-package meow-command
  :config
  (defun gs-101/meow-super-prev ()
    "Runs different upwards navigation commands based on the current major or minor mode.

- `combobulate-mode' :: `combobulate-navigate-up'
- `markdown-mode' :: `markdown-previous-visible-heading'
- `org-mode' :: `org-previous-visible-heading'
- `prog-mode' :: `backward-up-list'
- Other :: `meow-prev-expand'"
    (interactive)
    (cond
     ((seq-some (lambda (mode) (string-match-p "combobulate" (symbol-name mode))) local-minor-modes) (combobulate-navigate-up))
     ((derived-mode-p 'markdown-mode) (markdown-previous-visible-heading 1))
     ((derived-mode-p 'org-mode) (org-previous-visible-heading 1))
     ((derived-mode-p 'prog-mode) (backward-up-list))
     (t (meow-prev-expand 1)))
    )

  (defun gs-101/meow-super-next ()
    "Runs different downwards navigation commands based on the current major or minor mode.

- `combobulate-mode' :: `combobulate-navigate-down'
- `markdown-mode' :: `markdown-next-visible-heading'
- `org-mode' :: `org-next-visible-heading'
- `prog-mode' :: `down-list'
- Other :: `meow-next-expand'"
    (interactive)
    (cond
     ((seq-some (lambda (mode) (string-match-p "combobulate" (symbol-name mode))) local-minor-modes) (combobulate-navigate-down))
     ((derived-mode-p 'markdown-mode) (markdown-next-visible-heading 1))
     ((derived-mode-p 'org-mode) (org-next-visible-heading 1))
     ((derived-mode-p 'prog-mode) (down-list))
     (t (meow-next-expand 1)))
    )

  (defun gs-101/meow-super-left ()
    "Runs different leftwards navigation commands based on the current major or minor mode.

- `combobulate-mode' :: `combobulate-navigate-previous'
- `prog-mode' :: `backward-sexp'
- `text-mode' :: `meow-back-word'
- Other :: `meow-left-expand'"
    (interactive)
    (cond
     ((seq-some (lambda (mode) (string-match-p "combobulate" (symbol-name mode))) local-minor-modes) (combobulate-navigate-previous))
     ((derived-mode-p 'prog-mode) (backward-sexp))
     ((derived-mode-p 'text-mode) (meow-back-word 1))
     (t (meow-left-expand)))
    )

  (defun gs-101/meow-super-right ()
    "Runs different rightwards navigation commands based on the current major or minor mode.

- `combobulate-mode' :: `combobulate-navigate-next'
- `prog-mode' :: `forward-sexp'
- `text-mode' :: `meow-next-word'
- Other :: `meow-right-expand'"
    (interactive)
    (cond
     ((seq-some (lambda (mode) (string-match-p "combobulate" (symbol-name mode))) local-minor-modes) (combobulate-navigate-next))
     ((derived-mode-p 'prog-mode) (forward-sexp))
     ((derived-mode-p 'text-mode) (meow-next-word 1))
     (t (meow-right-expand)))
    )
  (defun gs-101/meow-super-kill ()
    "Runs different kill commands based on the current major or minor mode.

- `combobulate-mode' :: `combobulate-kill-node-dwim'
- `prog-mode' :: `kill-sexp'
- Other :: `meow-kill-whole-line'"
    (interactive)
    (cond
     ((seq-some (lambda (mode) (string-match-p "combobulate" (symbol-name mode))) local-minor-modes) (combobulate-kill-node-dwim))
     ((derived-mode-p 'prog-mode) (kill-sexp))
     (t (meow-kill-whole-line)))
    )
  (defun gs-101/meow-super-mark ()
    "Runs different mark commands based on the current major or minor mode.

- `combobulate-mode' :: `combobulate-mark-node-dwim'
- `prog-mode' :: `meow-block'
- Other :: `meow-mark-word'"
    (interactive)
    (cond
     ((seq-some (lambda (mode) (string-match-p "combobulate" (symbol-name mode))) local-minor-modes) (combobulate-mark-node-dwim))
     ((derived-mode-p 'prog-mode) (meow-block 1))
     (t (meow-mark-word 1)))
    )
  (defun gs-101/meow-transpose ()
    "Runs different transposition commands based on the current major or minor mode.

- `combobulate-mode' :: `combobulate-transpose-sexps'
- `prog-mode' :: `meow-transpose-sexp'
- Other :: `transpose-words'"
    (interactive)
    (cond
     ((seq-some (lambda (mode) (string-match-p "combobulate" (symbol-name mode))) local-minor-modes) (combobulate-transpose-sexps))
     ((derived-mode-p 'prog-mode) (meow-transpose-sexp))
     (t (transpose-words)))
    )
  (defvar gs-101/meow-beginning-of-line "C-a"
    "KBD macro for command`beginning-of-line'.")
  (defun gs-101/meow-super-beginning ()
    "Runs different beginning commands depending on current major mode.
- `prog-mode' :: `meow-back-to-indentation'
- Other :: `beginning-of-line'"
    (interactive)
    (cond
     ((derived-mode-p 'prog-mode) (meow-back-to-indentation))
     (t (meow--execute-kbd-macro gs-101/meow-beginning-of-line))))
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
   '("<" . beginning-of-buffer)
   '(">" . end-of-buffer)
   '("'" . repeat)
   '("=" . meow-indent)
   '("|" . align-regexp)
   '("<escape>" . ignore)
   '("?" . meow-comment)
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
   '("a" . gs-101/meow-super-beginning)
   '("A" . meow-beginning-of-thing)
   '("b" . meow-left)
   '("B" . gs-101/meow-super-left)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("e" . move-end-of-line)
   '("E" . meow-end-of-thing)
   '("f" . meow-right)
   '("F" . gs-101/meow-super-right)
   '("g" . meow-grab)
   '("G" . meow-swap-grab)
   '("h" . meow-mark-word)
   '("H" . gs-101/meow-super-mark)
   '("i" . meow-insert)
   '("I" . meow-append)
   '("j" . meow-pop-to-mark)
   '("J" . meow-pop-to-global-mark)
   '("k" . meow-kill)
   '("K" . gs-101/meow-super-kill)
   '("l" . meow-visual-line)
   '("L" . meow-visual-line-expand)
   '("n" . meow-next)
   '("N" . gs-101/meow-super-next)
   '("p" . meow-prev)
   '("P" . gs-101/meow-super-prev)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-query-replace-regexp)
   '("s" . isearch-forward)
   '("t" . gs-101/meow-transpose)
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

(use-package meow-command
  :after meow avy
  :bind
  (
   :map meow-normal-state-keymap
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
   (alist-get ?K avy-dispatch-alist) #'gs-101/meow-avy-action-kill-whole-line
   (alist-get ?m avy-dispatch-alist) #'gs-101/meow-avy-action-block
   )

  (defun favetelinguis/meow-jumper (&optional arg)
    "Switch between Meow search and Avy,
depending on if the region is active.
If the region is active, this function calls `meow-search'.
Otherwise, it calls `avy-goto-char-timer'."
    (interactive)
    (if (region-active-p)
        (meow-search arg)
      (avy-goto-char-timer)))
  )

(use-package meow-tree-sitter
  :vc (:url "https://github.com/skissue/meow-tree-sitter")
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
