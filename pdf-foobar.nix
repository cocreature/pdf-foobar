{ mkDerivation, aeson, base, containers, servant, servant-server
, stdenv
}:
mkDerivation {
  pname = "pdf-foobar";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers servant servant-server
  ];
  homepage = "https://github.com/cocreature/pdf-foobar";
  license = stdenv.lib.licenses.bsd3;
}
