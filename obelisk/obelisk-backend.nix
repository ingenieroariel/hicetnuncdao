{ mkDerivation, base, bytestring, categories, containers, cookie
, data-default, dependent-sum, dependent-sum-template, fetchgit
, lens, mtl, obelisk-asset-serve-snap
, obelisk-executable-config-inject
, obelisk-executable-config-lookup, obelisk-frontend, obelisk-route
, obelisk-snap-extras, reflex-dom, snap, snap-server, stdenv, text
, universe
}:
mkDerivation {
  pname = "obelisk-backend";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/obsidiansystems/obelisk";
    sha256 = "1wm2q4blqga6appp193idkapnqsan7qnkz29kylqag1y11fk4rrj";
    rev = "d9df151ed175be4f2dff721676e412a88a0596c1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/backend; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring categories containers cookie data-default
    dependent-sum dependent-sum-template lens mtl
    obelisk-asset-serve-snap obelisk-executable-config-inject
    obelisk-executable-config-lookup obelisk-frontend obelisk-route
    obelisk-snap-extras reflex-dom snap snap-server text universe
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
