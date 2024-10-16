# Add your reusable NixOS modules to this directory, on their own file (https://nixos.wiki/wiki/Module).
# These should be stuff you would like to share with others, not your personal configurations.
{
  # List your module files here
  fonts = import ./fonts.nix;
  audio = import ./audio.nix;
  xserver = import ./xserver.nix;
  keyd = import ./keyd.nix;
  mullvad = import ./mullvad.nix;
  bqn = import ./bqn;
}
