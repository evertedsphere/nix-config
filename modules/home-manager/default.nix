# Add your reusable home-manager modules to this directory, on their own file (https://nixos.wiki/wiki/Module).
# These should be stuff you would like to share with others, not your personal configurations.
{
  # List your module files here
  # my-module = import ./my-module.nix;
  local = import ./local.nix;
  keyd-application-mapper = import ./keyd-application-mapper.nix;
  org-protocol = import ./org-protocol.nix;
}
