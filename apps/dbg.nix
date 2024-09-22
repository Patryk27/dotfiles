{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      gdb
      kcachegrind
      linuxPackages.perf
      valgrind
      wireshark
    ];
  };
}
