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

(package! evil-terminal-cursor-changer
  :recipe (:host github :repo "7696122/evil-terminal-cursor-changer")
  :pin "2358f3e27d89128361cf80fcfa092fdfe5b52fd8")

(package! focus
  :recipe (:host github :repo "larstvei/Focus")
  :pin "17c471544f540f2cf9a05fd6cd87e52e5de317e2")

(package! indent-bars
  :recipe (:host github :repo "jdtsmith/indent-bars")
  :pin "2d1d854ddaa5b0e19b69e73553675c2aaaed1641")

(package! kkp
  :recipe (:host github :repo "benjaminor/kkp")
  :pin "ed9214329f11b095fc7bad06feb329b9f232258d")

(package! lsp-mode
  :pin "02c5ba59ce3d1cede4aa689c530f16cccfb5e1d1")

(package! org-modern
  :recipe (:host github :repo "minad/org-modern")
  :pin "0b7af08548e586c0d3b0ca4a683253da407220d1")

(package! rustic
  :recipe (:host github :repo "emacs-rustic/rustic")
  :pin "39423d1cf4fa054c36bf9577356451f4c06ee148")

(package! vlf
  :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c")
