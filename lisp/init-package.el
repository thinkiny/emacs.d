;;; init-package.el --- package related things -*- lexical-binding: t -*-

;; (setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
;;                          ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;; Fire up package.el
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package fullframe
  :config
  (fullframe list-packages quit-window))

(use-package cl-lib
  :ensure nil)

(use-package gnu-elpa-keyring-update)

(setq package-native-compile t)
(use-proxy-local 'package--with-response-buffer-1)

(provide 'init-package)
