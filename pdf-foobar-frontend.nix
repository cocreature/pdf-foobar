{ mkDerivation, aeson, base, containers, ghcjs-base, miso, stdenv
}:
mkDerivation {
  pname = "pdf-foobar";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers ghcjs-base miso
  ];
  homepage = "https://github.com/cocreature/pdf-foobar";
  license = stdenv.lib.licenses.bsd3;
}
