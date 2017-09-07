{ pkgs ? import <nixpkgs> {} }:
let
  ghcjs = pkgs.haskell.packages.ghcjsHEAD;
  ghc = pkgs.haskell.packages.ghc802;
  miso-ghcjs = ghcjs.callPackage ./miso-ghcjs.nix {};
  mupdf = pkgs.callPackage ./mupdf.nix {};
in
  rec {
    frontend = pkgs.haskell.lib.disableSharedExecutables (ghcjs.callPackage ./pdf-foobar-frontend.nix { miso = miso-ghcjs; });
    css = ./assets/style.css;
    backend = pkgs.haskell.lib.disableSharedExecutables (ghc.callPackage ./pdf-foobar-backend.nix {});
    docker-image = pkgs.dockerTools.buildImage {
      name = "cocreature/pdf-foobar";
      contents = [backend mupdf.bin];
      runAsRoot = ''
        mkdir -p /tmp /data
      '';
      extraCommands = ''
        echo "foobar";
        cp "${frontend}"/bin/pdf-foobar-frontend.jsexe/* data/;
        cp "${css}" data/style.css;
      '';
      config = {
        Cmd = [ "/bin/pdf-foobar-backend" "-d" "/data" ];
        ExposedPorts = {
          "3000/tcp" = {};
        };
      };
    };
  }
