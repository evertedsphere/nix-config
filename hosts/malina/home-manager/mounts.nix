{
  config,
  lib,
  pkgs,
  ...
}: {
  systemd.user.services.mount-seedbox = let
    mountDir = "${config.home.homeDirectory}/mnt/seedbox";
  in {
    Unit.Description = "Mount seedbox locally via rclone";
    Install.WantedBy = ["default.target"];
    Service = {
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p ${mountDir}";
      ExecStart = ''
        ${pkgs.rclone}/bin/rclone mount seedbox: ${mountDir} \
         --allow-other \
         --allow-non-empty \
         --log-level=INFO \
         --buffer-size=1G \
         --no-modtime
      '';
      ExecStop = "${pkgs.fuse}/bin/fusermount -u ${mountDir}";
      Type = "notify";
      Restart = "always";
      RestartSec = "10s";
      Environment = ["PATH=/run/wrappers/bin/:$PATH"];
    };
  };
}
