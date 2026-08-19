;; -*- lexical-binding: t; -*-

(use-package leetcode
  :vc (:url "https://github.com/thinkiny/leetcode.el" :rev "master")
  :config
  (setq leetcode-prefer-language "golang")
  (setq leetcode-save-solutions t)
  (setq leetcode-prefer-sql "mysql")
  (apply-reader-keybindings leetcode--problem-detail-mode-map))

(provide 'init-leetcode)
