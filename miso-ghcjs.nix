{ mkDerivation, aeson, base, bytestring, containers, ghcjs-base
, http-api-data, http-types, network-uri, scientific, servant
, stdenv, text, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "miso";
  version = "0.7.5.0";
  sha256 = "1nfnsmhiqncahasm21y3m5626ra9glq2nvwjv15a5i6h3qw5m328";
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
