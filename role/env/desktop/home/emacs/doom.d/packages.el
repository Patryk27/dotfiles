;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! bracketed-paste)

(package! point-history
  :recipe (:host github :repo "blue0513/point-history")
  :pin "dd6d42cfbb25b3432786b263451f433a68eebabf")

(package! ivy-point-history
  :recipe (:host github :repo "SuzumiyaAoba/ivy-point-history")
  :pin "5d450a91656503c98a548557a563ee99d65b46b3")
