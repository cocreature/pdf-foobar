{ pkgs ? import <nixpkgs> {} }:
let
  ghcjs = pkgs.haskell.packages.ghcjsHEAD;
  miso-ghcjs = ghcjs.callPackage ./miso-ghcjs.nix {};
in
  {
    client = ghcjs.callPackage ./pdf-foobar.nix { miso = miso-ghcjs; };
  }
