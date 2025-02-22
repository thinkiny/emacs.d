;;; init-gptel.el --- init-gptel -*- lexical-binding: t -*-

(use-package gptel
  :bind (:map global-map
              ("C-c <RET>"  . 'gptel-send-or-query)
              ("C-c y n" . 'gptel-with-option)
              ("C-c y m" . 'gptel-menu)
              ("C-c y t" . 'gptel-org-set-topic)
              ("C-c y c" . 'elysium-open))

  :config
  (unbind-key "\C-c <RET>" 'gptel-mode-map)
  (setq
   gptel-model 'coder
   gptel-backend (gptel-make-openai "Coder"
                   :protocol "http"
                   :host "localhost:1234"
                   :stream t
                   :models '(coder)))

  (defun gptel-send-or-query()
    (interactive)
    (if (string= "*elysium*" (buffer-name))
        (call-interactively 'elysium-query)
      (call-interactively 'gptel-send)))

  (defun gptel-with-option()
    (interactive)
    (let ((current-prefix-arg 4))
      (call-interactively 'gptel))))

(use-package elysium
  :demand t
  :config
  (setq elysium-window-size 0.4)

  (defun advice/after-elysium-setup-windows()
    (with-current-buffer elysium--chat-buffer
      (setq-local gptel--system-message
                  (alist-get 'programming gptel-directives))))

  (defun elysium-open()
    (interactive)
    (if (buffer-live-p elysium--chat-buffer)
        (call-interactively #'elysium-menus/body)
      (call-interactively #'elysium-query)))
  (advice-add 'elysium-setup-windows :after #'advice/after-elysium-setup-windows))

(defhydra elysium-menus (:color blue :hint nil)
  "
-----------------------------------------------------------------
q: query             t: toggle
c: clear             a: add context
u: ignore all        k: keep all
"
  ("t" #'elysium-toggle-window)
  ("q" #'elysium-query)
  ("c" #'elysium-clear-buffer)
  ("u" #'elysium-discard-all-suggested-changes)
  ("k" #'elysium-keep-all-suggested-changes))

(provide 'init-gptel)
