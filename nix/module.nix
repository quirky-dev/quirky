{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.quirky;

  settingsFormat = pkgs.formats.yaml {};

  configFile = settingsFormat.generate "quirky-config.yaml" cfg.settings;

in {
  options.services.quirky = {
    enable = mkEnableOption "Quirky health monitoring";

    package = mkOption {
      type = types.package;
      description = "The Quirky package to use";
    };

    settings = mkOption {
      type = settingsFormat.type;
      default = {};
      description = ''
        Quirky configuration as a Nix attrset.
        See example-satellite.yaml and example-aggregator.yaml for options.
      '';
      example = literalExpression ''
        {
          satellite = {
            bind = "0.0.0.0";
            port = 9855;
            checks = {
              disk_space = {
                actions = [{
                  name = "check";
                  action = {
                    type = "external";
                    path = "./check-disk.sh";
                  };
                  config = {
                    path = "/";
                    threshold = 90;
                  };
                }];
              };
            };
          };
        }
      '';
    };

    user = mkOption {
      type = types.str;
      default = "quirky";
      description = "User account under which Quirky runs";
    };

    group = mkOption {
      type = types.str;
      default = "quirky";
      description = "Group under which Quirky runs";
    };

    extraGroups = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Additional groups for the Quirky user (e.g., for accessing secrets)";
    };

    scriptPackages = mkOption {
      type = types.listOf types.package;
      default = with pkgs; [ coreutils jq bash procps ];
      description = ''
        Packages to add to PATH for external action scripts.
        Default includes coreutils, jq, bash, and procps.
        Note: sudo is provided via /run/wrappers/bin/sudo automatically.
      '';
    };

    allowSudo = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Allow sudo usage in health check scripts by disabling NoNewPrivileges.
        This is needed for PostgreSQL health checks that run commands as postgres user.
        Only enable if you need to run checks as other users.
      '';
    };
  };

  config = mkIf cfg.enable {
    systemd.services.quirky = {
      description = "Quirky Health Monitoring";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      # Add wrappers directory first for setuid programs like sudo
      path = [ "/run/wrappers" ] ++ cfg.scriptPackages;

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.group;
        ExecStart = "${cfg.package}/bin/quirky ${configFile}";
        Restart = "on-failure";
        RestartSec = "10s";

        # Logging
        StandardOutput = "journal";
        StandardError = "journal";

        # Security hardening
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        NoNewPrivileges = !cfg.allowSudo;  # Disable if sudo is needed
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
        RestrictRealtime = true;
        RestrictNamespaces = true;
        LockPersonality = true;
        MemoryDenyWriteExecute = false;  # Haskell GHC RTS needs this
        RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
      };
    };

    # Only create user and group if using default "quirky" user
    users.users = mkMerge [
      (mkIf (cfg.enable && cfg.user == "quirky") {
        quirky = {
          isSystemUser = true;
          group = cfg.group;
          extraGroups = cfg.extraGroups;
          description = "Quirky health monitoring user";
        };
      })
    ];

    users.groups = mkMerge [
      (mkIf (cfg.enable && cfg.group == "quirky") {
        quirky = {};
      })
    ];

    # Allow quirky user to run health check commands as postgres
    security.sudo.extraConfig = mkIf (cfg.enable && cfg.allowSudo) ''
      ${cfg.user} ALL=(postgres) NOPASSWD: ALL
    '';
  };
}
