{ mkDerivation
, base, bytestring, containers, cookie, dependent-sum
, fetchgit
, ghcjs-dom, jsaddle, lens, mtl
, obelisk-executable-config-inject
, obelisk-executable-config-lookup, primitive
, ref-tf, reflex, reflex-dom-core
, stdenv
, text, transformers
#, obelisk-route
}:
mkDerivation {
  pname = "obelisk-frontend";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/obsidiansystems/obelisk";
    sha256 = "1wm2q4blqga6appp193idkapnqsan7qnkz29kylqag1y11fk4rrj";
    rev = "d9df151ed175be4f2dff721676e412a88a0596c1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/frontend; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring containers cookie dependent-sum ghcjs-dom jsaddle
    lens mtl obelisk-executable-config-inject
    obelisk-executable-config-lookup primitive ref-tf
    reflex reflex-dom-core text transformers
#   obelisk-route
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
