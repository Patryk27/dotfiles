(package! atomic-chrome
  :recipe (:host github :repo "alpha22jp/atomic-chrome")
  :pin "f1b077be7e414f457191d72dcf5eedb4371f9309")

(package! dirvish
  :recipe (:host github :repo "alexluigit/dirvish")
  :pin "f85a88216dfc6e952854595281b56c5b7b716557")

(package! eat
  :recipe (:type git
           :host nil
           :repo "ssh://gitlab@gitlab.pwy.io:47000/pwy/emacs-eat.git"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el")))
  :pin "c0193e0e4f429a3f95e63a4afea678593f4a096b")

(package! evil-escape
  :disable t)

(package! focus
  :recipe (:host github :repo "larstvei/Focus")
  :pin "29b412b209c3542a7932c201f0166e48c9fd7fee")

(package! indent-bars
  :recipe (:host github :repo "jdtsmith/indent-bars")
  :pin "8a1252ff36f6a41e51861cf03e95d52181d38ea6")

(package! kkp
  :recipe (:host github :repo "benjaminor/kkp")
  :pin "ad23d961c3a5dce83b1c9a6b4c65b48809c7af9a")

(package! nix-mode
  :recipe (:host github :repo "Patryk27/nix-mode")
  :pin "f1d588051d4fdd54c88eb7fb4a92b8352fb24f83")

(package! pdf-tools
  :built-in 'prefer)

(package! rustic
  :recipe (:host github :repo "emacs-rustic/rustic")
  :pin "22a5ef8bfd5a34ced945c2722938eb29632371d4")

(package! track-changes
  :built-in t)

(package! tree-sitter-langs
  :pin "365a4f7bf5184d04b5cc48175d93d7af7b8bbeb4")
