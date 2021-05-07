{ mkDerivation, base, fetchgit, lens, stdenv }:
mkDerivation {
  pname = "tabulation";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/obsidiansystems/obelisk";
    sha256 = "1wm2q4blqga6appp193idkapnqsan7qnkz29kylqag1y11fk4rrj";
    rev = "d9df151ed175be4f2dff721676e412a88a0596c1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/tabulation; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base lens ];
  description = "A library for types that can be completely reconstructed by fields as specified by a GADT";
  license = stdenv.lib.licenses.bsd3;
}
