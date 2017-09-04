{ mkDerivation, aeson, base, bytestring, containers, http-media
, optparse-applicative, process, servant, servant-multipart
, servant-server, stdenv, temporary, text, warp
}:
mkDerivation {
  pname = "pdf-foobar";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers http-media optparse-applicative
    process servant servant-multipart servant-server temporary text
    warp
  ];
  homepage = "https://github.com/cocreature/pdf-foobar";
  license = stdenv.lib.licenses.bsd3;
}
