{ pkgs ? import <nixpkgs> {} }:
let
  ghcjs = pkgs.haskell.packages.ghcjsHEAD;
  ghc = pkgs.haskell.packages.ghc802;
  miso-ghcjs = ghcjs.callPackage ./miso-ghcjs.nix {};
in
  {
    frontend = ghcjs.callPackage ./pdf-foobar-frontend.nix { miso = miso-ghcjs; };
    backend = ghc.callPackage ./pdf-foobar-backend.nix {};
  }
