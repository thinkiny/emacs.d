(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-nt* (eq system-type 'windows-nt))

(if *is-a-mac*
    (require 'init-mac))

(if *is-a-linux*
    (require 'init-linux))

(if *is-a-nt*
    (require 'init-nt))

(provide 'init-platform)
