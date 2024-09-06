{pkgs, ...}: {
  home.packages = [
    pkgs.emacs-lsp-booster
  ];

  programs.doom-emacs = {
    enable = true;
    doomDir = ../doom;
    # As of 31-08-2024, Emacs 31 is way snappier overall but also produces tons
    # of multiple-second pauses doing god knows what (I couldn't be arsed to
    # check with the profiler) and once sat there spinning for a whole five
    # minutes trying to render my agenda. It's unusable.
    # Also segfaults if you profile it sometimes.
    # Update: this was almost certainly tree-sitter's fault, not that of Emacs itself

    # This nonsense is for lsp-booster; the init.el trick doesn't work for us
    # (see: https://discourse.doomemacs.org/t/using-lsp-use-plists-with-rust-analyzer-stops-updating-diagnostics-on-save/2832/3)
    # (not unless you're lucky enough to use it on a first build of nix-doom-unstraightened)
    emacs = pkgs.symlinkJoin {
      name = "emacs";
      paths = [pkgs.emacs-unstable];
      nativeBuildInputs = [pkgs.makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/emacs --set LSP_USE_PLISTS true
        wrapProgram $out/bin/emacsclient --set LSP_USE_PLISTS true
      '';
      inherit (pkgs.emacs-unstable) meta src;
    };
  };

  # https://github.com/marienz/nix-doom-emacs-unstraightened/blob/cfd0a1ad0995efc8ee1149e09445bcde9f9b0a8d/README.md
  # If you set `services.emacs.enable = true`, that will run Unstraightened as well
  # (Unstraightened sets itself as `services.emacs.package`). Set
  # `programs.doom-emacs.provideEmacs = false` or override `services.emacs.package`
  # if you want a vanilla Emacs daemon instead.
  services.emacs.enable = false;
}
