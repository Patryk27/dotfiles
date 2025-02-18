(package! atomic-chrome
  :recipe (:host github :repo "alpha22jp/atomic-chrome")
  :pin "f1b077be7e414f457191d72dcf5eedb4371f9309")

(package! dirvish
  :recipe (:host github :repo "Patryk27/dirvish")
  :pin "f986123a4d3247c9c3a8186ee51d797c8fda820f")

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

(package! focus
  :recipe (:host github :repo "larstvei/Focus")
  :pin "29b412b209c3542a7932c201f0166e48c9fd7fee")

(package! indent-bars
  :recipe (:host github :repo "jdtsmith/indent-bars")
  :pin "8a1252ff36f6a41e51861cf03e95d52181d38ea6")

(package! nix-mode
  :recipe (:host github :repo "Patryk27/nix-mode")
  :pin "2ff9d0958de5512e8a5c5cba63448c880fa71c91")

(package! pdf-tools
  :built-in 'prefer)

(package! rustic
  :recipe (:host github :repo "emacs-rustic/rustic")
  :pin "9fdf5c76b20cfc2985d518b3a4ae7b9634b39999")

(package! tree-sitter-langs
  :pin "365a4f7bf5184d04b5cc48175d93d7af7b8bbeb4")
