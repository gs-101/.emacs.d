;;; -*- lexical-binding: t -*-

(use-package puni
  :bind
  (
   :map puni-mode-map
   ([remap forward-sentence] . puni-end-of-sexp)
   ([remap backward-sentence] . puni-beginning-of-sexp)
   ("M-k" . kill-sexp)
   ([remap mark-paragraph] . puni-expand-region)
   ([remap kill-line] . puni-kill-line)
   ([remap forward-sexp] . puni-forward-sexp-or-up-list)
   ([remap backward-sexp] . puni-backward-sexp-or-up-list)
   ([remap mark-sexp] . puni-mark-sexp-at-point)
   ([remap kill-sexp] . Gavinok/puni-kill-thing-at-point)
   ([remap transpose-sexps] . puni-transpose)
   ("C-)" . puni-slurp-forward)
   ("C-(" . puni-slurp-backward)
   ("C-}" . puni-barf-forward)
   ("C-{" . puni-barf-backward)
   )
  :defer t
  :config
  (defun gs-101/puni-wrap-pick (&optional n)
    "Completing read interface for wrapping S-expressions.
If a ‘C-u’ prefix argument is given, wrap all S-expressions
following the point until the end of the buffer or of the
enclosing list.  If a numeric prefix argument N is given, wrap N
S-expressions. Automatically indent the newly wrapped
S-expression."
    (interactive "P")
    (let ((choice (completing-read "Choose a wrapper: "
                                   '("Angle" "Curly" "Round" "Square"))))
      (pcase choice
        ("Angle" (puni-wrap-angle n))
        ("Curly" (puni-wrap-curly n))
        ("Round" (puni-wrap-round n))
        ("Square" (puni-wrap-square n)))))
  (defun Gavinok/puni-kill-thing-at-point (&optional arg)
    "Kill the next puni based thing at point."
    (interactive)
    (unless buffer-read-only
      (puni-expand-region)
      (kill-region (region-beginning) (region-end))))
  (advice-add #'puni-kill-active-region :override
              (defun AmaiKinono/puni-kill-active-region ()
                "Kill active region.
When this will cause unbalanced state, ask the user to confirm,
unless `puni-confirm-when-delete-unbalanced-active-region' is
nil.
When `rectangle-mark-mode' is enabled, kill the marked
rectangular region instead."
                (interactive)
                (if (use-region-p)
                    (puni-kill-region)
                  ;; Fall back to Emacs default behavior which is signaling an error or what
                  ;; `kill-region-dwim' defines (since Emacs 31).
                  (call-interactively #'kill-region))))
  :ensure t
  :custom
  (cursor-type 'bar)
  :init
  (puni-global-mode)
  )

(provide 'gs-keys-puni)
