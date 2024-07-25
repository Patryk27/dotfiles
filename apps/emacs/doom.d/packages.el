(package! atomic-chrome
  :recipe (:host github :repo "alpha22jp/atomic-chrome")
  :pin "f1b077be7e414f457191d72dcf5eedb4371f9309")

(package! company
  :pin "c67946d6e32060a046543411ce93d1656c2c882c")

(package! dirvish
  :pin "119f9f59a618bb7b476c93e9ab1d7542c5c1df41")

(package! eat
  :recipe (:type git
           :host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el")))
  :pin "3a6f418f55d183b9d86f99c140caed4ba3d44f93")

(package! focus
  :recipe (:host github :repo "larstvei/Focus")
  :pin "17c471544f540f2cf9a05fd6cd87e52e5de317e2")

(package! indent-bars
  :recipe (:host github :repo "jdtsmith/indent-bars")
  :pin "2d1d854ddaa5b0e19b69e73553675c2aaaed1641")

(package! lsp-mode
  :pin "52987755175042b9569e32d72ae29eba119020d6")

(package! pdf-tools
  :built-in 'prefer)

(package! rustic
  :recipe (:host github :repo "emacs-rustic/rustic")
  :pin "c1893528ad6b8e8ca795b547f326ec52dacf8728")

(package! straight
  :recipe (:host github :repo "Patryk27/straight.el")
  :pin "3468d18d3ed83a48325901e1c88473acf427a4fe")

(package! vlf
  :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c")
