# [[file:~/repos/neurosys/README.org::*settings.nix][settings.nix:1]]
{config, pkgs, lib, ...}:

with lib;

{
  options = {
    settings = {
      name = mkOption {
        default = "Dan Girshovich";
        type = with types; uniq str;
      };
      username = mkOption {
        default = "dan";
        type = with types; uniq str;
      };
      email = mkOption {
        default = "dan.girsh@gmail.com";
        type = with types; uniq str;
      };
    };
  };
}
# settings.nix:1 ends here
