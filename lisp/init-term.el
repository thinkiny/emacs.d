;;Remote Directory Tracking: https://www.emacswiki.org/emacs/AnsiTermHints#h5o-5
(add-hook 'term-mode-hook (lambda ()
                            ;; Hack to set two escape chars.
                            (let (term-escape-char)
                              (term-set-escape-char ?\C-x))
                            (let (term-escape-char)
                              (term-set-escape-char ?\C-c))
                            (define-key term-raw-map (kbd "C-c C-y") 'term-paste)
                            (define-key term-raw-map (kbd "C-c C-k") 'term-char-mode)
                            (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)))

(require 'counsel-term)
(global-set-key (kbd "C-x t") 'counsel-term)

(provide 'init-term)
