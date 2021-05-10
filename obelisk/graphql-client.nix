{ mkDerivation, aeson, aeson-schemas, base, bytestring, fetchgit
, file-embed, hpack, http-client, http-client-tls, http-types, mtl
, optparse-applicative, path, path-io, stdenv, tasty, tasty-hunit
, template-haskell, text, transformers, typed-process
, unliftio-core
}:
mkDerivation {
  pname = "graphql-client";
  version = "1.1.1";
  src = fetchgit {
    url = "https://github.com/LeapYear/graphql-client";
    sha256 = "0lvb0jlamzcysnfad1bslaj5mh4hqmzhygrq7axkcass8xmmad0h";
    rev = "5847ffd2514c78adbc05e618ec77e34780168ea8";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/graphql-client; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-schemas base http-client http-client-tls http-types mtl
    template-haskell text transformers unliftio-core
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-schemas base bytestring file-embed http-client
    http-client-tls http-types mtl optparse-applicative path path-io
    template-haskell text transformers typed-process unliftio-core
  ];
  testHaskellDepends = [
    aeson aeson-schemas base http-client http-client-tls http-types mtl
    tasty tasty-hunit template-haskell text transformers unliftio-core
  ];
  prePatch = "hpack";
  homepage = "https://github.com/LeapYear/graphql-client#readme";
  description = "A client for Haskell programs to query a GraphQL API";
  license = stdenv.lib.licenses.bsd3;
}
