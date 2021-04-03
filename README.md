# dotfiles

Configurations for some of my NixOS machines.

[#flakes](https://nixos.wiki/wiki/Flakes), [#home-manager](https://github.com/nix-community/home-manager), [#wayland](https://nixos.wiki/wiki/Sway), [#sops](https://github.com/Mic92/sops-nix)

# Notable parts

## Alacritty

- [nix](role/env/desktop/home/alacritty.nix)

## Backup

- [nix](role/app/backup.nix)
- [script](role/app/backup/backup.sh)

## Emacs

- [nix](role/env/desktop/home/emacs.nix)
- [doom.d](role/env/desktop/home/emacs/doom.d)

I'm using GccEmacs + Pure Gtk from [emacs-overlay](https://github.com/nix-community/emacs-overlay); as for the configuration itself, it's [Doom Emacs](https://github.com/hlissner/doom-emacs).

Because [nix-doom-emacs](https://github.com/vlaci/nix-doom-emacs) doesn't work with GccEmacs yet, as a temporary solution I'm building a script called [`doom-sync`](https://github.com/Patryk27/dotfiles/blob/4ae3d7dab9bb474c7d0afab328544abbf7f32da9/role/env/desktop/home/emacs.nix#L10) that runs `git checkout ... && doom sync -up` - I launch that script manually after `nixos-rebuild switch`.

## Rust

- [nix](role/env/default/home/rust.nix) (with sccache!)

## Sway

- [nix](role/env/desktop/home/sway.nix)
- [config](role/env/desktop/home/sway/config)

## Waybar

- [nix](role/env/desktop/home/waybar.nix)
- [style](role/env/desktop/home/waybar/style.css)

## ZSH

- [nix](role/env/default/home/zsh.nix)
- [p10k](role/env/default/home/zsh/p10k.zsh)
- [miscellaneous stuff](role/env/default/home/zsh/autorun)

# Structure

### `node`

My machines.

Two of them are physical laptops, the third one is a virtual machine where I offload CPU-intensive tasks.

### `role/app`

Applications that wouldn't make sense to push upstream (i.e. my backup script).

### `role/env`

Environments: `default` contains all the non-graphical applications (because one of my nodes is a _headless_ virtual machine), `desktop` contains the rest.

### `role/target`

Deployment targets: physical machine and virtual machine.

# License

Copyright (c) 2021, Patryk Wychowaniec <pwychowaniec@pm.me>.    
Licensed under the MIT license.
