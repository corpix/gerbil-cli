{
  inputs = {
    nixpkgs.url = "tarball+https://git.tatikoma.dev/corpix/nixpkgs/archive/v2024-05-29.632320.tar.gz";
    gerbil.url = "tarball+https://git.tatikoma.dev/corpix/gerbil-nix/archive/v2024-07-07.32.tar.gz";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, gerbil }:
    flake-utils.lib.eachDefaultSystem
      (arch:
        let
          pkgs = nixpkgs.legacyPackages.${arch}.pkgs;

          inherit (builtins)
            removeAttrs
          ;

          inherit (pkgs)
            writeText
            writeScript
            stdenv
          ;
          inherit (pkgs.lib)
            attrValues
            filter
          ;

          packages = [
            pkgs.coreutils
            pkgs.gnumake
            pkgs.git
            pkgs.gcc
            # pkgs.glibc.static
            # (pkgs.zlib.override { shared = false; static = true; })
            # (pkgs.openssl.override { static = true; })
            # (pkgs.sqlite.overrideAttrs (super: { configureFlags = super.configureFlags ++ ["--enable-static" "--disable-shared"]; }))
            pkgs.glibc
            pkgs.zlib
            pkgs.openssl
            pkgs.sqlite
            gerbil.packages.${arch}.gerbil-static
          ];

        in {
          packages.default = gerbil.stdenv.${arch}.static.mkGerbilPackage {
            name = "gerbil-cli";
            src = ./.;
            buildPhase = ''
              make
            '';
            installPhase = ''
              mkdir -p $out/gerbil
              mv .gerbil/lib $out/gerbil
            '';
          };

          devShells.default = pkgs.mkShell {
            name = "gerbil-cli";
            packages = packages;
          };
        });
}
