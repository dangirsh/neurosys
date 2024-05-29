# [[id:55b66861-0727-404b-b10b-f9adb7151e5e][Global Constants:1]]
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
# Global Constants:1 ends here
