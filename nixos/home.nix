# [[file:~/repos/neurosys/README.org::*home.nix][home.nix:1]]
{ config, pkgs, ... }:

let
  homeDir = builtins.getEnv "HOME";
  syncDir = builtins.toPath("${homeDir}/Sync");
  sources = import ./nix/sources.nix;
  nixos20_03 = import sources."nixpkgs-20.03" { };
  emacs-overlay = import (import ./nix/sources.nix)."emacs-overlay";
in {
  imports = [
    ./settings.nix
  ];


  home.stateVersion = "20.03";

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: { stable = nixos20_03; };
  };

  nixpkgs.overlays = [ emacs-overlay ];

  services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    # Compile with imagemagick support so I can resize images.
    package = pkgs.emacsGit.override { inherit (pkgs) imagemagick; };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.sessionVariables = {
    EDITOR = "emacsclient --create-frame --alternate-editor emacs";
    PASSWORD_STORE_DIR = "${syncDir}/.password-store";
    GNUPGHOME = "${syncDir}/.gnupg/";
    # GTK2_RC_FILES="${homeDir}/.gtkrc-2.0";
    # https://github.com/xmonad/xmonad/issues/126
    _JAVA_AWT_WM_NONREPARENTING = "1";
  };

  # gtk = {
  #   enable = true;
  #   iconTheme = {
  #     name = "Adwaita";
  #     package = pkgs.gnome3.adwaita-icon-theme;
  #   };
  #   theme = {
  #     name = "Adwaita-dark";
  #     package = pkgs.gnome3.gnome_themes_standard;
  #   };
  # };

  xdg.enable = true;

  home.packages = with pkgs; [
    # Haskell dev
    # haskellPackages.ghcid
    # haskellPackages.hakyll
    # haskellPackages.hasktags
    # haskellPackages.hlint
    # haskellPackages.hoogle
    # haskellPackages.hpack
    # cabal-install
    # stable.haskellPackages.apply-refact # used by hlint-refactor
    # stable.haskellPackages.brittany

    # cabal2nix
    # cachix
    # nix-prefetch-git
    # nixfmt

    rofi

    gnupg

    pavucontrol
    # syncthing-cli # provides stcli
    # vlc
    xdotool

    (pass.withExtensions (exts: [
      exts.pass-otp
      exts.pass-genphrase
    ]))

    gitAndTools.hub

    firefox-beta-bin
    # firefox-bin

    # direnv

    # sbcl
    # lispPackages.quicklisp

    # clojure
    # joker
    # leiningen

    # julia_13

    ## Doom dependencies
    # emacsGit

    git
    (ripgrep.override {withPCRE2 = true;})
    gnutls              # for TLS connectivity

    ## Optional dependencies
    fd                  # faster projectile indexing
    imagemagick         # for image-dired
    pinentry_emacs

    ## Module dependencies
    # :tools lookup & :lang org +roam
    sqlite
    # :lang latex & :lang org (latex previews)
    texlive.combined.scheme-tetex
  ];

  programs.bash = {
    enable = true;
    historyFile = "${syncDir}/.config/bash/.bash_history";
    sessionVariables = {
      PATH = "\$PATH:${homeDir}/.emacs.d/bin/";
    };
    shellOptions = [
    "autocd" "cdspell" "dirspell" "globstar" # bash >= 4
    "cmdhist" "nocaseglob" "histappend" "extglob"];
  };

  programs.git = {
    enable = true;
    userName = "${config.settings.name}";
    userEmail = "${config.settings.email}";
  };

  # programs.direnv.enable = true;

  programs.ssh = {
    enable = true;

    controlMaster  = "auto";
    controlPath    = "/tmp/ssh-%u-%r@%h:%p";
    controlPersist = "1800";

    forwardAgent = true;
    serverAliveInterval = 60;

    hashKnownHosts = true;
    userKnownHostsFile = "${homeDir}/.ssh/known_hosts";

    matchBlocks = {
      droplet = {
        hostname = "45.55.5.197";
        identityFile = "${homeDir}/.ssh/id_rsa";
        user = "dgirsh";
      };
      dangirsh = {
        host = "dangirsh.org";
        hostname = "ssh.phx.nearlyfreespeech.net";
        identityFile = "${homeDir}/.ssh/id_rsa";
        user = "dangirsh_dangirsh";
      };
      nixos-dev = {
        hostname = "45.79.58.229";
        identityFile = "${homeDir}/.ssh/id_rsa";
        user = "dan";
      };
    };
  };

  # services.redshift = {
  #   enable = true;
  #   latitude = "33";
  #   longitude = "-97";
  #   temperature.day = 6500;
  #   temperature.night = 3000;
  # };

  # https://www.reddit.com/r/emacsporn/comments/euf7m8/doomoutrunelectric_theme_xmonad_nixos/
  # https://github.com/willbush/system/blob/371cfa9933f24bca585a3c6c952c41c864d97aa0/nixos/home.nix#L178
  # services.compton = {
  #     enable = true;
  #     fade = true;
  #     backend = "xrender";
  #     fadeDelta = 1;
  #     # I only want transparency for a couple of applications.
  #     opacityRule = [
  #       "90:class_g ?= 'emacs' && focused"
  #       "75:class_g ?= 'emacs' && !focused"
  #       "90:class_g ?= 'alacritty' && focused"
  #       "75:class_g ?= 'alacritty' && !focused"
  #     ];
  #   };

  # services.syncthing.enable = true;
  # services.lorri.enable = true;
}
# home.nix:1 ends here
