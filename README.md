# dotfiles

Configurations for some of my NixOS machines.

[#flakes](https://nixos.wiki/wiki/Flakes), [#home-manager](https://github.com/nix-community/home-manager), [#wayland](https://nixos.wiki/wiki/Sway), [#sops](https://github.com/Mic92/sops-nix)

# Notable parts

## Alacritty

- [nix](role/env/desktop/home/alacritty.nix)

## Emacs

- [nix](role/env/desktop/home/emacs.nix)
- [doom.d](role/env/desktop/home/emacs/doom.d)

## Sway

- [nix](role/env/desktop/home/sway.nix)
- [config](role/env/desktop/home/sway/config)

## Waybar

- [nix](role/env/desktop/home/waybar.nix)
- [style](role/env/desktop/home/waybar/style.css)

## Vim

- [nix](role/env/default/home/vim.nix)

## ZSH

- [nix](role/env/default/home/zsh.nix)

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
