# [[file:~/repos/neurosys/README.org::*User-level Config][User-level Config:1]]
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
# User-level Config:1 ends here

# [[file:~/repos/neurosys/README.org::*Environment Variables][Environment Variables:1]]
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
# Environment Variables:1 ends here

# [[file:~/repos/neurosys/README.org::*Packages][Packages:1]]
home.packages = with pkgs; [
  rofi
  gnupg
  # syncthing-cli # provides stcli

  (pass.withExtensions (exts: [
    exts.pass-otp
    exts.pass-genphrase
  ]))

  firefox-beta-bin

  xtrlock-pam  # screen locking
  maim  # screenshots
  rofi-pass  # interface to password manager
  xclip  # programmatic access to clipbaord
  arandr  # gui for xrandr (monitor layout)

  # direnv

  # Upstream failing
  # julia_13

  ## Doom dependencies

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
# Packages:1 ends here

# [[file:~/repos/neurosys/README.org::*Programs][Programs:1]]
programs = {

  # Let Home Manager install and manage itself.
  home-manager.enable = true;

  emacs = {
    enable = true;
    # Compile with imagemagick support so I can resize images.
    package = pkgs.emacsGit.override { inherit (pkgs) imagemagick; };
  };

  bash = {
    enable = true;
    historyFile = "${syncDir}/.config/bash/.bash_history";
    # FIXME: Document and reduce these
    shellOptions = [
      "autocd" "cdspell" "dirspell" "globstar" # bash >= 4
      "cmdhist" "nocaseglob" "histappend" "extglob"];
  };

  git = {
    enable = true;
    userName = "${config.settings.name}";
    userEmail = "${config.settings.email}";
  };

  # direnv.enable = true;

  ssh = {
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
}
# Programs:1 ends here

# [[file:~/repos/neurosys/README.org::*Services][Services:1]]
services = {
  emacs.enable = true;

  # redshift = {
  #   enable = true;
  #   latitude = "33";
  #   longitude = "-97";
  #   temperature.day = 6500;
  #   temperature.night = 3000;
  # };

  # https://www.reddit.com/r/emacsporn/comments/euf7m8/doomoutrunelectric_theme_xmonad_nixos/
  # https://github.com/willbush/system/blob/371cfa9933f24bca585a3c6c952c41c864d97aa0/nixos/home.nix#L178
  # compton = {
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

  # syncthing.enable = true;
  # lorri.enable = true;
};
# Services:1 ends here

# [[file:~/repos/neurosys/README.org::*Services][Services:2]]

# Services:2 ends here
