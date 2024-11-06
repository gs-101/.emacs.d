;;; -*- lexical-binding: t -*-

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :commands
  (mu4e)
  :custom
  (mu4e-compose-format-flowed t)
  (mu4e-change-filenames-when-moving t)
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-maildir (expand-file-name "!Mail-Provider/Personal/" mail-source-directory))
  (mu4e-view-show-addresses 't)
  (mu4e-view-show-images t)
  (mu4e-drafts-folder "/Drafts")
  (mu4e-sent-folder "/Sent")
  (mu4e-refile-folder "/All Mail")
  (mu4e-trash-folder "/Trash")
  (mu4e-maildir-shortcuts '(
                            ("/Inbox" . ?i)
                            ("/Drafts" . ?d)
                            ("/Sent" . ?s)
                            ("/Trash" . ?t)
                            ("/All Mail" . ?a)
                            ))
  )

(use-package mu4e-column-faces
  :after mu4e
  :config
  (mu4e-column-faces-mode)
  :ensure t
  )

(use-package simple
  :after mu4e
  :custom
  (mail-user-agent 'mu4e-user-agent)
  )

(use-package message
  :after mu4e
  :custom
  (message-kill-buffer-on-exit t)
  (message-mail-user-agent t)
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-smtp-server "!mailprovider")
  (smtpmail-local-domain "!domain")
  (smtpmail-smtp-service 465)
  (smtpmail-stream-type 'ssl)
  )

(use-package startup
  :custom
  (user-mail-address "!email")
  :defer t
  )

(use-package org-msg
  :after mu4e
  :custom
  (org-msg-greeting-fmt "\nGreetings,\n\n")
  (org-msg-default-alternatives '(
                                  (new . (text html))
                                  (reply-to-html . (text html))
                                  (reply-to-text . (text))
                                  ))
  (org-msg-convert-citation t)
  (org-msg-signature
   "Regards,

#+begin_signature
--
*Gabriel*
#+end_signature"
   )
  :ensure t
  :config
  (org-msg-mode)
  )

(use-package jabber
  :bind-keymap
  ("C-c j" . jabber-global-keymap)
  :defer t
  :ensure t
  )

(provide 'gs-chat)
