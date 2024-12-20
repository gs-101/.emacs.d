;;; -*- lexical-binding: t -*-

(use-package puni
  :bind
  (
   :map puni-mode-map
   ("C-M-a" . puni-beginning-of-sexp)
   ("C-M-e" . puni-end-of-sexp)
   ("M-h" . puni-expand-region)
   ("M-H" . puni-contract-region)
   ("C-k" . puni-kill-line)
   ("C-M-n" . puni-forward-sexp-or-up-list)
   ("C-M-p" . puni-backward-sexp-or-up-list)
   ("C-M-<right>" . puni-forward-sexp-or-up-list)
   ("C-M-<left>" . puni-backward-sexp-or-up-list)
   ("C-M-@" . puni-mark-sexp-at-point)
   ("C-M-SPC" . puni-mark-sexp-at-point)
   ("M-k" . Gavinok/puni-kill-thing-at-point)
   ("C-M-t" . puni-transpose)
   ("C-)" . puni-slurp-forward)
   ("C-(" . puni-slurp-backward)
   ("C-M-)" . puni-barf-forward)
   ("C-M-(" . puni-barf-backward)
   )
  :defer t
  :config
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
  :hook
  (text-mode . puni-disable-puni-mode)
  :init
  (puni-global-mode)
  )

(provide 'gs-keys-puni)
