# [[file:../README.org::*Hardware-specific Config][Hardware-specific Config:1]]
# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
    ];

  boot.initrd.availableKernelModules = [ "virtio_pci" "ahci" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  nix.maxJobs = lib.mkDefault 1;
# Hardware-specific Config:1 ends here

# [[file:../README.org::*Disk Mounts][Disk Mounts:1]]
  fileSystems."/" =
    { device = "/dev/disk/by-uuid/bf38bdde-34dd-4d57-9bfe-07de465f0f29";
      fsType = "ext4";
    };

  # Linode Volume "bkp". Targetted by syncthing.
  fileSystems."/bkp" =
    { device = "/dev/disk/by-id/scsi-0Linode_Volume_bkp";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/7596d600-d2c6-4d77-b138-7f595283af00"; }
    ];
}
# Disk Mounts:1 ends here
