{ mkDerivation, attoparsec, base, bytestring, containers, directory
, fetchgit, filepath, obelisk-snap-extras, snap, snap-core, stdenv
, text, transformers, unix
}:
mkDerivation {
  pname = "obelisk-asset-serve-snap";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/obsidiansystems/obelisk";
    sha256 = "1wm2q4blqga6appp193idkapnqsan7qnkz29kylqag1y11fk4rrj";
    rev = "d9df151ed175be4f2dff721676e412a88a0596c1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/asset/serve-snap; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    attoparsec base bytestring containers directory filepath
    obelisk-snap-extras snap snap-core text transformers unix
  ];
  description = "Serve preprocessed assets using Snap";
  license = stdenv.lib.licenses.bsd3;
}
