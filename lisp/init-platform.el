;; -*- lexical-binding: t; -*-

(if *is-a-mac*
    (require 'init-mac))

(if *is-a-linux*
    (require 'init-linux))

(if *is-a-nt*
    (require 'init-nt))

(provide 'init-platform)
