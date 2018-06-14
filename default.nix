{ rev ? "f35cc5eb42006429738b2866951c06d632301605",
  outputSha256 ? "1k1lnlip5rl705dycsbqnz4sdnh61xdsj2c62sby0pzhjppn4sjs"
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
  ghcjs = pkgs.haskell.packages.ghcjs80.extend (self: super: {
    servant = super.callHackage "servant" "0.12.1" {};
    http-types = super.callHackage "http-types" "0.11" {};
    miso = super.callCabal2nix "miso" miso-src {};
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
