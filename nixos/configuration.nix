# [[file:~/repos/neurosys/README.org::*configuration.nix][configuration.nix:1]]
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

  nixpkgs.config = {
    # Allow unfree, which is required for some drivers.
    allowUnfree = true;
  };

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

  time.timeZone = "America/Los_Angeles";

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

  networking.useDHCP = false;
  networking.usePredictableInterfaceNames = false;
  networking.interfaces.eth0.useDHCP = true;
  networking.firewall.enable = true;
  # networking.networkmanager.enable = true;
  networking.hostName = "nixos-dev";

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
  };

  # virtualisation.docker.enable = true;

  environment.systemPackages = with pkgs; [
    coreutils
    binutils
    curl
    wget
    zip
    unzip
    # docker
    # docker-compose
    # ghcide-nix.ghcide-ghc865
    tree
    git
    killall
    unzip
    wget
    sshfs
    gnumake
    mtr
    sysstat
    htop
  ];

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      hack-font
    ];
  };

  system.stateVersion = "20.03";

  # services.xserver.autorun = true;

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

  programs.x2goserver.enable = true;
}
# configuration.nix:1 ends here
