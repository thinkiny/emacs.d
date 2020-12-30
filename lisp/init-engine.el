(require-package 'engine-mode)

(after-load 'engine-mode
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s")

  (defengine bing
    "http://global.bing.com/search?q=%s"
    :keybinding "b")

  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (defengine scala
    "http://www.scala-lang.org/api/2.13.1/index.html?search=%s"
    :keybinding "s")

  (defengine cpp
    "https://en.cppreference.com/mwiki/index.php?search=%s"
    :keybinding "c")

  (defengine rfcs
    "http://pretty-rfc.herokuapp.com/search?q=%s")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")

  (defengine wiktionary
    "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s"))

(unless window-system
  (setq engine/browser-function 'eww-browse-url))

(require 'engine-mode)
(engine-mode t)
(provide 'init-engine)
