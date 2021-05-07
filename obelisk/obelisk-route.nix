{ mkDerivation, aeson, base, bytestring, categories, constraints
, constraints-extras, containers, dependent-map, dependent-sum
, dependent-sum-template, either, fetchgit, ghcjs-dom, http-types
, jsaddle, lens, monad-control, mtl, network-uri
, obelisk-executable-config-lookup, primitive, ref-tf, reflex
, reflex-dom-core, stdenv, tabulation, template-haskell, text
, th-extras, transformers, universe, universe-dependent-sum
}:
mkDerivation {
  pname = "obelisk-route";
  version = "0.2";
  src = fetchgit {
    url = "https://github.com/obsidiansystems/obelisk";
    sha256 = "1wm2q4blqga6appp193idkapnqsan7qnkz29kylqag1y11fk4rrj";
    rev = "d9df151ed175be4f2dff721676e412a88a0596c1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/route; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base bytestring categories constraints constraints-extras
    containers dependent-map dependent-sum dependent-sum-template
    either ghcjs-dom http-types jsaddle lens monad-control mtl
    network-uri obelisk-executable-config-lookup primitive ref-tf
    reflex reflex-dom-core tabulation template-haskell text th-extras
    transformers universe universe-dependent-sum
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
