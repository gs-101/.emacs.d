;;; -*- lexical-binding: t -*-

(use-package message
  :custom
  (message-kill-buffer-on-exit t)
  (message-mail-user-agent t)
  (message-send-mail-function #'smtpmail-send-it)
  (smtpmail-smtp-service 465)
  (smtpmail-stream-type 'ssl)
  (smtpmail-smtp-server "disroot.org"))

(use-package startup
  :custom
  (user-mail-address "gabrielsantosdesouza@disroot.org")
  :defer t)

(provide 'gs-chat)
