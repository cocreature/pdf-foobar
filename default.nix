{ pkgs ? import <nixpkgs> {} }:
let
  ghcjs = pkgs.haskell.packages.ghcjsHEAD;
  ghc = pkgs.haskell.packages.ghc802;
  miso-ghcjs = ghcjs.callPackage ./miso-ghcjs.nix {};
in
  rec {
    frontend = pkgs.haskell.lib.disableSharedExecutables (ghcjs.callPackage ./pdf-foobar-frontend.nix { miso = miso-ghcjs; });
    backend = pkgs.haskell.lib.disableSharedExecutables (ghc.callPackage ./pdf-foobar-backend.nix {});
    docker-image = pkgs.dockerTools.buildImage {
      name = "cocreature/pdf-foobar";
      contents = [frontend backend pkgs.mupdf.bin];
      runAsRoot = ''
        mkdir -p /tmp
      '';
    };
  }
