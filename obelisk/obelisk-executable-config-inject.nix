{ mkDerivation, base, base64-bytestring, bytestring, containers
, fetchgit, reflex-dom-core, stdenv, text
}:
mkDerivation {
  pname = "obelisk-executable-config-inject";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/obsidiansystems/obelisk";
    sha256 = "1wm2q4blqga6appp193idkapnqsan7qnkz29kylqag1y11fk4rrj";
    rev = "d9df151ed175be4f2dff721676e412a88a0596c1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/executable-config/inject; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base base64-bytestring bytestring containers reflex-dom-core text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
