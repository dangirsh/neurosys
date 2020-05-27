# [[file:~/repos/neurosys/README.org::*System-level Config][System-level Config:1]]
{ config, pkgs, ... }:
let
  sources = import ./nix/sources.nix;
  # ghcide-nix = import sources."ghcide-nix" { };
in {
  imports =
    [ ./hardware-configuration.nix
      ./settings.nix
      "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/release-20.03.tar.gz}/nixos"
    ];

  system.stateVersion = "20.03";

  nixpkgs.config = {
    # Allow unfree, which is required for some drivers.
    allowUnfree = true;
  };
# System-level Config:1 ends here

# [[file:~/repos/neurosys/README.org::*Nix][Nix:1]]
nix = {
  useSandbox = true;
  autoOptimiseStore = true;
  maxJobs = 3; # should be 1 per CPU logical core
  binaryCaches = [
    "https://cache.nixos.org/"
    "https://ghcide-nix.cachix.org"
    "https://hercules-ci.cachix.org"
    "https://iohk.cachix.org"
    "https://nix-tools.cachix.org"
  ];
  binaryCachePublicKeys = [
    "ghcide-nix.cachix.org-1:ibAY5FD+XWLzbLr8fxK6n8fL9zZe7jS+gYeyxyWYK5c="
    "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
    "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
    "nix-tools.cachix.org-1:ebBEBZLogLxcCvipq2MTvuHlP7ZRdkazFSQsbs0Px1A="
  ];
  gc = {
    automatic = true;
    dates = "23:00";
    options = "--delete-older-than 30d";
  };
};
# Nix:1 ends here

# [[file:~/repos/neurosys/README.org::*Timezone][Timezone:1]]
time.timeZone = "America/Los_Angeles";
# Timezone:1 ends here

# [[file:~/repos/neurosys/README.org::*Boot][Boot:1]]
boot = {
  cleanTmpDir = true;

  loader = {
    timeout = 1; # Timeout (in seconds) until loader boots the default menu item.
    grub = {
      enable = true;
      version = 2;
      device = "nodev";
      copyKernels = true;
      fsIdentifier = "provided";
      extraConfig = "serial; terminal_input serial; terminal_output serial";
    };
    systemd-boot.enable = false;
    efi.canTouchEfiVariables = false;

  };
};
# Boot:1 ends here

# [[file:~/repos/neurosys/README.org::*Networking][Networking:1]]
networking.useDHCP = false;
networking.usePredictableInterfaceNames = false;
networking.interfaces.eth0.useDHCP = true;
networking.firewall.enable = true;
# networking.networkmanager.enable = true;
networking.hostName = "nixos-dev";
# Networking:1 ends here

# [[file:~/repos/neurosys/README.org::*Services][Services:1]]
services = {

  xserver = {
    enable = true;
    layout = "us";

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackges: [
        haskellPackges.xmonad-contrib
        haskellPackges.xmonad-extras
        haskellPackges.xmonad
      ];
    };

    displayManager = {
      defaultSession = "none+xmonad";
      lightdm.enable = true;
    };
    desktopManager.xterm.enable = false;
  };
# Services:1 ends here

# [[file:~/repos/neurosys/README.org::*Syncthing][Syncthing:1]]
  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/networking/syncthing.nix
  syncthing = {
    enable = true;
    openDefaultPorts = true;
    user = "${config.settings.username}";
    configDir = "/home/${config.settings.username}/.config/syncthing";
    dataDir = "/home/${config.settings.username}/.local/share/syncthing";
    declarative = {
      devices = {
        nixos-dev.id = "EEMRJQE-TBONTUL-UBGJ6FT-AAUS25K-COP3VHE-WERN7IN-PTNZ63Z-GZZX2AY";
        x1carbon.id = "IOINCUM-QKL34MC-RSPJETE-CUW5D2Y-3CMDSLD-72HNXZI-7U3TN44-35CLKQN";
        pixel3a-2.id = "NT5ZJ55-JBWGL4D-WIK57V3-T3RXF2Q-HLMHV7U-DLGAZ5U-ZSHSPYZ-M5NLQAC";
      };
      folders = {
        sync = rec {
          id = "at23u-zmxto";
          devices = [ "nixos-dev" "x1carbon"  "pixel3a-2"];
          path = "/bkp/Sync";
          watch = false;
          rescanInterval = 3600 * 1;
          type = "receiveonly"; # sendreceive
          enable = true;
          versioning.type = "simple";
          versioning.params.keep = "5";
        };
        media = rec {
          id = "media";
          devices = [ "nixos-dev"  "x1carbon" ];
          path = "/bkp/Media";
          watch = false;
          rescanInterval = 3600 * 6;
          type = "receiveonly"; # sendreceive
          enable = true;
          versioning.type = "simple";
          versioning.params.keep = "5";
        };
        work = rec {
          id = "work";
          devices = [ "nixos-dev" "x1carbon" ];
          path = "/bkp/Work";
          watch = false;
          rescanInterval = 3600 * 6;
          type = "receiveonly"; # sendreceive
          enable = true;
          versioning.type = "simple";
          versioning.params.keep = "5";
        };
      };
    };
  };

};

# virtualisation.docker.enable = true;
# Syncthing:1 ends here

# [[file:~/repos/neurosys/README.org::*Packages][Packages:1]]
environment.systemPackages = with pkgs; [
  coreutils binutils
  curl wget
  zip unzip
  git
  killall
  syncthing-cli
  sshfs
  mtr # traceroute
  sysstat
  htop
];
# Packages:1 ends here

# [[file:~/repos/neurosys/README.org::*Fonts][Fonts:1]]
fonts = {
  enableFontDir = true;
  enableGhostscriptFonts = true;
  fonts = with pkgs; [
    corefonts
    hack-font
  ];
};
# Fonts:1 ends here

# [[file:~/repos/neurosys/README.org::*User Definition][User Definition:1]]
security.sudo.wheelNeedsPassword = false;

users.mutableUsers = false;

users.extraUsers.${config.settings.username} = {
  isNormalUser = true;
  uid = 1000;
  createHome = true;
  home = "/home/${config.settings.username}";
  description = "${config.settings.name}";
  extraGroups = [
    "audio"
    "networkmanager"
    "systemd-journal"
    "vboxusers"
    "video"
    "wheel"
  ];
};

home-manager.users.dan = import ./home.nix ;
# User Definition:1 ends here

# [[file:~/repos/neurosys/README.org::*SSH][SSH:1]]
services.openssh = {
  enable = true;
  forwardX11 = true;
  permitRootLogin = "without-password";
  passwordAuthentication = false;
};

users.users.${config.settings.username}.openssh.authorizedKeys.keys = [
  "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC+yJ5sv7iO9PBuozfmitR0JJfqDsJ7w+rlryq5CwdatO3tkRdR5dMYdFTFCeHbmeakPTC/uys08fziEUXh3DL206jDKQEMBoMGXNowZHyYzr25nIogHbveqeNTgP8jsTw5uBaJu8LFzHHey4Sw9WlRrvIqguUT5jB3omZh8yDWcxTrTJlTsN2TM3HILvirfVwBkD2uNTDdd5LplbZhx6x87VCs6ZNYhBjJ4CPcO4zTQuEdyyxUHEgtMkYgrS4Jb/Kl6Tleftlh55E74SZ3XXnw3lWdH9ra8ewH265iqNr/RwysagnalslBZDLl8yJcrMsCVi4tPrZZc4vaeCsIWK4X dan@x1carbon"
];

programs.ssh.startAgent = true;
# SSH:1 ends here

# [[file:~/repos/neurosys/README.org::*X2Go Client][X2Go Client:1]]
  # programs.x2goserver.enable = true;
# X2Go Client:1 ends here

# [[file:~/repos/neurosys/README.org::*End][End:1]]
}
# End:1 ends here
