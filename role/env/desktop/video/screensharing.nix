{ config, pkgs, ... }: {
  boot = {
    extraModulePackages = [
      config.boot.kernelPackages.v4l2loopback
    ];

    extraModprobeConfig = ''
      options v4l2loopback exclusive_caps=1 video_nr=9 card_label=a7III
    '';

    kernelModules = [
      "v4l2loopback"
    ];
  };

  environment = {
    systemPackages = [
      (pkgs.writeShellScriptBin "share-screen" ''
        ${pkgs.wf-recorder}/bin/wf-recorder \
          --muxer=v4l2 \
          --file="/dev/video9" \
          -c rawvideo \
          -x yuyv422
      '')
    ];
  };
}
