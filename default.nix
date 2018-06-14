{ rev ? "3e3a9e661d7ef83f9dfc26d948a12f8ee1334f6d",
  outputSha256 ? "07rhxdig4l0xlpwdg4z0vsz4k3gvkd32bzi13mymv4mn9hbqwb4w"
}:
let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = outputSha256;
  };
  pkgs = import nixpkgs {};
  miso-src = pkgs.fetchFromGitHub {
      owner = "dmjio";
      repo = "miso";
      rev = "d6fd3fa96cf5adca9052460328be888c64df52d8";
      sha256 = "0v4p41i45mnhpfrqw3n0394w0g9pglylxy66vbaqyfx6vvmbwzs4";
  };
  ghcjs = pkgs.haskell.packages.ghcjs84.extend (self: super: {
    doctest = null;
    ghcjs-base = pkgs.haskell.lib.dontCheck (super.callCabal2nix "ghcjs-base" (pkgs.fetchFromGitHub {
      owner = "ghcjs";
      repo = "ghcjs-base";
      rev = "4560541fbab5b77ee34d0348ec04bb1dda8e5db3";
      sha256 = "0n7m46f32iass89ww4f2c126k4yqhfp95km7c9l6klsh93lp1i8p";
    }) {});
    http-media = pkgs.haskell.lib.dontCheck super.http-media;
    http-types = pkgs.haskell.lib.dontCheck super.http-types;
    miso = super.callCabal2nix "miso" miso-src {};
    scientific = pkgs.haskell.lib.dontCheck super.scientific;
    servant = pkgs.haskell.lib.dontCheck super.servant;
    stm = super.callHackage "stm" "2.4.5.0" {};
    syb = pkgs.haskell.lib.dontCheck super.syb;
    tasty-quickcheck = pkgs.haskell.lib.dontCheck super.tasty-quickcheck;
  });
  ghc = pkgs.haskell.packages.ghc843.extend (self: super: {
    miso = super.callCabal2nix "miso" miso-src {};
  });
in
  rec {
    frontend = ghcjs.callCabal2nix "pdf-foobar" ./. {};
    assets = ./assets;
    backend =  ghc.callCabal2nix "pdf-foobar" ./. {};
    pid1 = pkgs.haskell.lib.justStaticExecutables ghc.pid1;
    docker-image = pkgs.dockerTools.buildImage {
      name = "cocreature/pdf-foobar";
      contents = [pid1 backend pkgs.mupdf];
      runAsRoot = ''
        mkdir -p /tmp /data
      '';
      extraCommands = ''
        echo "foobar";
        cp "${frontend}"/bin/pdf-foobar-frontend.jsexe/* data/;
        cp -r "${assets}" data/assets;
      '';
      config = {
        Entrypoint = [ "/bin/pid1" ];
        Cmd = [ "/bin/pdf-foobar-backend" "-d" "/data" ];
        ExposedPorts = {
          "3000/tcp" = {};
        };
      };
    };
  }
