{ mkDerivation, aeson, base, bytestring, containers, ghcjs-base
, http-api-data, http-types, network-uri, scientific, servant
, stdenv, text, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "miso";
  version = "0.8.0.0";
  sha256 = "1cc956bb6cee1847c6d5dfbfba021ac32d7dd5cf8e96baa799d081f1466b89fc";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers ghcjs-base http-api-data
    http-types network-uri scientific servant text transformers
    unordered-containers vector
  ];
  homepage = "http://github.com/dmjio/miso";
  description = "A tasty Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
