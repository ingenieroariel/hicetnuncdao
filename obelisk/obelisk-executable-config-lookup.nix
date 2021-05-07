{ mkDerivation, base, base64-bytestring, bytestring, containers
, directory, fetchgit, filepath, jsaddle, monad-control, primitive
, ref-tf, reflex, reflex-dom, stdenv, text, transformers
, transformers-base
}:
mkDerivation {
  pname = "obelisk-executable-config-lookup";
  version = "0.1.1";
  src = fetchgit {
    url = "https://github.com/obsidiansystems/obelisk";
    sha256 = "1wm2q4blqga6appp193idkapnqsan7qnkz29kylqag1y11fk4rrj";
    rev = "d9df151ed175be4f2dff721676e412a88a0596c1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/executable-config/lookup; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base base64-bytestring bytestring containers directory filepath
    jsaddle monad-control primitive ref-tf reflex reflex-dom text
    transformers transformers-base
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
