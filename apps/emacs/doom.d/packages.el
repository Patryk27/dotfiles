(package! alert
  :recipe (:host github :repo "jwiegley/alert")
  :pin "7774b5fd2feb98d4910ff06435d08c19fba93e26")

(package! atomic-chrome
  :recipe (:host github :repo "alpha22jp/atomic-chrome")
  :pin "f1b077be7e414f457191d72dcf5eedb4371f9309")

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

(package! vlf
  :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c")

(package! kkp
  :recipe (:host github :repo "benjaminor/kkp"))

(package! lsp-mode
  :pin "02c5ba59ce3d1cede4aa689c530f16cccfb5e1d1")

(package! parrot
  :recipe (:host github :repo "dp12/parrot")
  :pin "1d381f24d74242018e306d1a0c891bed9a465ac3")

(package! rustic
  :pin "39423d1cf4fa054c36bf9577356451f4c06ee148")
