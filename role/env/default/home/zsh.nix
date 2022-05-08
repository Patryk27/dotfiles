{ pkgs, ... }: {
  environment = {
    # For autosuggestions
    pathsToLink = [ "/share/zsh" ];
  };

  programs = {
    autojump = {
      enable = true;
    };
  };

  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        # For fzf-history-widget
        perl
      ];
    };

    programs = {
      zsh = {
        enable = true;
        enableAutosuggestions = true;
        enableCompletion = true;

        history = {
          share = false;
        };

        oh-my-zsh = {
          enable = true;
          theme = "custom";

          plugins = [
            "autojump"
            "extract"
            "fzf"
            "git"
          ];

          extraConfig = ''
            FZF_BASE="$(fzf-share)"
            ZSH_CUSTOM="${./zsh/custom}"

            COMPLETION_WAITING_DOTS="true"
            DISABLE_MAGIC_FUNCTIONS="true"

            eval "$(direnv hook zsh)"

            ${pkgs.any-nix-shell}/bin/any-nix-shell zsh --info-right | source /dev/stdin
            source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
            source ${./zsh/autorun.zsh}
          '';
        };

        shellAliases = {
          c = "docker-compose";
          ca = "clear && cargo";
          cab = "clear && cargo build";
          cabr = "clear && cargo build --release";
          cac = "clear && cargo check";
          caf = "clear && cargo fmt";
          car = "clear && cargo run";
          carb = "clear && RUST_BACKTRACE=1 cargo run";
          carr = "clear && cargo run --release";
          carrb = "clear && RUST_BACKTRACE=1 cargo run --release";
          cate = "clear && cargo test --quiet";
          cateb = "clear && RUST_BACKTRACE=1 cargo test";
          catew = "clear && cargo test --quiet --workspace";
          catewb = "clear && RUST_BACKTRACE=1 cargo test --workspace";
          catewf = "clear && cargo test --all-features --quiet --workspace";
          catewfb = "clear && RUST_BACKTRACE=1 cargo test --all-features --workspace";
          cau = "clear && cargo update";
          caup = "clear && cargo update --package";
          d = "docker";
          jcl = "journalctl";
          jclu = "journalctl --user";
          nx = "nix develop -c zsh";
          scl = "systemctl";
          sclu = "systemctl --user";
          vg-cg = "valgrind --tool=callgrind --dump-instr=yes --simulate-cache=yes --collect-jumps=yes -- ";
        };
      };
    };
  };
}
