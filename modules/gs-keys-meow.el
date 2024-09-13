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
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
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
   '("j" . meow-next)
   '("k" . meow-prev)
   )
  )

(use-package meow-helpers
  :config
  (meow-normal-define-key
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("'" . repeat)
   '("=" . meow-indent)
   '("<escape>" . ignore)
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
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   )
  )

(use-package meow-thing
  :config
  (meow-thing-register 'arrow '(pair ("<") (">")) '(pair ("<") (">")))
  )

(use-package meow-tree-sitter
  :ensure t
  :config
  (meow-tree-sitter-register-defaults)
  )

(provide 'gs-keys-meow)
