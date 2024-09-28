{ pkgs, user, ... }: {
  programs = {
    zsh = {
      enable = true;
    };
  };

  home-manager.users."${user}" = {
    home = {
      packages = with pkgs; [
        autojump
        perl
      ];
    };

    programs = {
      zsh = {
        enable = true;
        enableCompletion = true;

        autosuggestion = {
          enable = true;
        };

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
          ];

          extraConfig = ''
            unsetopt PROMPT_SP

            COMPLETION_WAITING_DOTS="true"
            DISABLE_MAGIC_FUNCTIONS="true"
            FZF_BASE="$(fzf-share)"
            ZSH_CUSTOM="${./zsh/custom}"

            eval "$(direnv hook zsh)"

            ${pkgs.any-nix-shell}/bin/any-nix-shell zsh --info-right | source /dev/stdin
            source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
            source ${./zsh/init.zsh}
          '';
        };

        shellAliases = {
          c = "docker-compose";
          ca = "clear && cargo";
          cab = "clear && cargo build";
          cabr = "clear && cargo build --release";
          cac = "clear && cargo check";
          cacw = "clear && cargo check --workspace";
          caf = "clear && cargo fmt";
          car = "clear && cargo run";
          carb = "clear && RUST_BACKTRACE=1 cargo run";
          carr = "clear && cargo run --release";
          carrb = "clear && RUST_BACKTRACE=1 cargo run --release";
          cate = "clear && cargo test --quiet";
          cateb = "clear && RUST_BACKTRACE=1 cargo test";
          cater = "clear && cargo test --quiet --release";
          catew = "clear && cargo test --quiet --workspace";
          catewr = "clear && cargo test --quiet --workspace --release";
          catewb = "clear && RUST_BACKTRACE=1 cargo test --workspace";
          catewf = "clear && cargo test --all-features --quiet --workspace";
          catewfb = "clear && RUST_BACKTRACE=1 cargo test --all-features --workspace";
          cau = "clear && cargo update";
          caup = "clear && cargo update --package";
          d = "docker";
          nx = "nix develop -c zsh";
        };
      };
    };
  };

  users = {
    users = {
      "${user}" = {
        shell = pkgs.zsh;
      };
    };
  };
}
