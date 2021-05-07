{ mkDerivation, attoparsec, base, bytestring, directory, fetchgit
, filepath, snap-core, stdenv, text, unix
}:
mkDerivation {
  pname = "obelisk-snap-extras";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/obsidiansystems/obelisk";
    sha256 = "1wm2q4blqga6appp193idkapnqsan7qnkz29kylqag1y11fk4rrj";
    rev = "d9df151ed175be4f2dff721676e412a88a0596c1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/snap-extras; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    attoparsec base bytestring directory filepath snap-core text unix
  ];
  description = "Extra functionality for Snap that should be considered for upstreaming";
  license = stdenv.lib.licenses.bsd3;
}
