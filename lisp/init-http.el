;;; init-http.el --- Work with HTTP APIs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package httprepl)

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode)
  :config
  (defun sanityinc/restclient ()
    (interactive)
    (with-current-buffer (get-buffer-create "*restclient*")
      (restclient-mode)
      (pop-to-buffer (current-buffer)))))


(provide 'init-http)
;;; init-http.el ends here
