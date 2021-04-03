{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [
      (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    {
      device = "rpool/root";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/F23D-9E7F";
      fsType = "vfat";
    };

  fileSystems."/camera" =
    {
      device = "rpool/camera";
      fsType = "zfs";
    };

  fileSystems."/downloads" =
    {
      device = "rpool/downloads";
      fsType = "zfs";
    };

  fileSystems."/home" =
    {
      device = "rpool/home";
      fsType = "zfs";
    };

  fileSystems."/secrets" =
    {
      device = "rpool/secrets";
      fsType = "zfs";
    };

  swapDevices =
    [
      { device = "/dev/disk/by-uuid/3ad1150f-77ff-407a-a97c-2342c7582cd6"; }
    ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.video.hidpi.enable = lib.mkDefault true;
}
