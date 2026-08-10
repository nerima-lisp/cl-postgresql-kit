{
  description = "A production-oriented PostgreSQL wire-protocol client for Common Lisp";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    cl-nix-forge = {
      url = "github:nerima-lisp/cl-nix-forge/v0.5.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.3.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-nix-forge.follows = "cl-nix-forge";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };

    paredit-cli = {
      url = "github:nerima-lisp/paredit-cli/v1.6.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };

    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/v0.5.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-nix-forge.follows = "cl-nix-forge";
      inputs.cl-weave.follows = "cl-weave";
      inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.paredit-cli.follows = "paredit-cli";
    };

    cl-json-kit = {
      url = "github:nerima-lisp/cl-json-kit/v1.2.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-nix-forge.follows = "cl-nix-forge";
      inputs.cl-weave.follows = "cl-weave";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };

    cl-date-kit = {
      url = "github:nerima-lisp/cl-date-kit/v1.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-nix-forge.follows = "cl-nix-forge";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };

    cl-boundary-kit = {
      url = "github:nerima-lisp/cl-boundary-kit/v2.3.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-nix-forge.follows = "cl-nix-forge";
      inputs.cl-weave.follows = "cl-weave";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };

    cl-concurrent-kit = {
      url = "github:nerima-lisp/cl-concurrent-kit/v0.6.1";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-nix-forge.follows = "cl-nix-forge";
      inputs.cl-weave.follows = "cl-weave";
      inputs.cl-boundary-kit.follows = "cl-boundary-kit";
      inputs.cl-date-kit.follows = "cl-date-kit";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };

    cl-resilience-kit = {
      url = "github:nerima-lisp/cl-resilience-kit/242cf254841741a180bfebfb55fbb8bb23bade07";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-nix-forge.follows = "cl-nix-forge";
      inputs.cl-weave.follows = "cl-weave";
      inputs.cl-boundary-kit.follows = "cl-boundary-kit";
      inputs.cl-concurrent-kit.follows = "cl-concurrent-kit";
      inputs.cl-date-kit.follows = "cl-date-kit";
      inputs.cl-observability-kit.follows = "cl-observability-kit";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };

    cl-log-kit = {
      url = "github:nerima-lisp/cl-log-kit/v2.2.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-nix-forge.follows = "cl-nix-forge";
      inputs.cl-weave.follows = "cl-weave";
      inputs.cl-json-kit.follows = "cl-json-kit";
      inputs.cl-date-kit.follows = "cl-date-kit";
      inputs.cl-concurrent-kit.follows = "cl-concurrent-kit";
      inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.paredit-cli.follows = "paredit-cli";
    };

    cl-observability-kit = {
      url = "github:nerima-lisp/cl-observability-kit/5d447256db014b8111b4441d1884bb5332447c9d";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-nix-forge.follows = "cl-nix-forge";
      inputs.cl-concurrent-kit.follows = "cl-concurrent-kit";
      inputs.cl-boundary-kit.follows = "cl-boundary-kit";
      inputs.cl-date-kit.follows = "cl-date-kit";
      inputs.cl-weave.follows = "cl-weave";
      inputs.cl-log-kit.follows = "cl-log-kit";
      inputs.paredit-cli.follows = "paredit-cli";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      cl-nix-forge,
      treefmt-nix,
      paredit-cli,
      cl-weave,
      cl-codec-kit,
      cl-json-kit,
      cl-date-kit,
      cl-concurrent-kit,
      cl-log-kit,
      cl-observability-kit,
      cl-resilience-kit,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      test-timeout-seconds = 120;
      coverage-timeout-seconds = 900;
      termination-grace-seconds = 10;
      coverage-entry-point-text = ''
        (load "scripts/run-coverage.lisp")
      '';
      cl = cl-nix-forge.lib.${nixpkgs.lib.head systems};
    in
    cl.mkPackageFlake {
      inherit self nixpkgs systems;

      pname = "cl-postgresql-kit";
      asd = ./cl-postgresql-kit.asd;
      root = ./.;
      timeoutSeconds = test-timeout-seconds;
      killAfterSeconds = termination-grace-seconds;
      meta = {
        description = "A PostgreSQL wire-protocol client for Common Lisp";
        homepage = "https://github.com/nerima-lisp/cl-postgresql-kit";
        license = nixpkgs.lib.licenses.mit;
      };

      docs.root = ./docs;
      treefmt.evalModule = treefmt-nix.lib.evalModule;

      lispDependencies = ctx: [
        cl-codec-kit.packages.${ctx.system}.cl-codec-kit
        cl-json-kit.packages.${ctx.system}.cl-json-kit
        cl-date-kit.packages.${ctx.system}.cl-date-kit
        cl-concurrent-kit.packages.${ctx.system}.cl-concurrent-kit
        cl-log-kit.packages.${ctx.system}.cl-log-kit
        cl-observability-kit.packages.${ctx.system}.cl-observability-kit
        cl-resilience-kit.packages.${ctx.system}.cl-resilience-kit
      ];

      lispCheckDependencies = ctx: [
        cl-weave.packages.${ctx.system}.cl-weave
      ];

      devShellPackages = ctx: [
        paredit-cli.packages.${ctx.system}.default
      ];

      extraOutputs = ctx: {
        packages.coverage = ctx.cl.mkCoverageReport {
          drv = ctx.package;
          entryPointText = coverage-entry-point-text;
          timeoutSeconds = coverage-timeout-seconds;
          killAfterSeconds = termination-grace-seconds;
        };
        checks.coverage = ctx.cl.mkCoverageReport {
          drv = ctx.package;
          entryPointText = coverage-entry-point-text;
          timeoutSeconds = coverage-timeout-seconds;
          killAfterSeconds = termination-grace-seconds;
        };
      };
    };
}
