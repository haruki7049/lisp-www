{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;
        treefmtEval = treefmt-nix.lib.evalModule pkgs ./treefmt.nix;

        lisp = pkgs.sbcl;
        lispPackages = pkgs.sbclPackages;
        pname = "lisp-www";
        version = "0.1.0";
        src = lib.cleanSource ./.;
        lispLibs = with lispPackages; [
          woo
        ];

        lisp-www = lisp.buildASDFSystem {
          inherit
            pname
            version
            src
            lispLibs
            ;
        };

        runtime = pkgs.sbcl.withPackages (ps: [
          ps.woo
          lisp-www
        ]);

        runner =
          let
            dependencies = [
              "woo"
              "lisp-www"
            ];

            asdf-loader = lib.strings.concatLines (map (x: "(asdf:load-system '${x})") dependencies);
          in
          pkgs.writeScriptBin "runner" ''
            #! ${runtime}/bin/sbcl --script
            (load (sb-ext:posix-getenv "ASDF"))
            ${asdf-loader}

            (lisp-www:main)
          '';
      in
      {
        formatter = treefmtEval.config.build.wrapper;

        packages = {
          inherit lisp-www runner;
          default = runner;
        };

        checks = {
          inherit lisp-www runtime runner;
          formatting = treefmtEval.config.build.check self;
        };

        devShells.default = pkgs.mkShell {
          packages = [
            # SBCL
            lisp

            # LSP
            pkgs.nil
          ];

          shellHook = ''
            export PS1="\n[nix-shell:\w]$ "
          '';
        };
      }
    );
}
