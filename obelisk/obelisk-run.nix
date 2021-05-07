{ mkDerivation, attoparsec, base, bytestring, containers, cookie
, dependent-sum, dependent-sum-template, fetchgit, ghcjs-dom
, HsOpenSSL, http-client, http-reverse-proxy, http-types, jsaddle
, jsaddle-warp, lens, modern-uri, mtl, network
, obelisk-asset-serve-snap, obelisk-backend
, obelisk-executable-config-lookup, obelisk-frontend, obelisk-route
, process, ref-tf, reflex, reflex-dom, snap-core, stdenv
, streaming-commons, text, time, universe, utf8-string, wai
, wai-websockets, warp, warp-tls, websockets, which
}:
mkDerivation {
  pname = "obelisk-run";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/obsidiansystems/obelisk";
    sha256 = "1wm2q4blqga6appp193idkapnqsan7qnkz29kylqag1y11fk4rrj";
    rev = "d9df151ed175be4f2dff721676e412a88a0596c1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/run; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    attoparsec base bytestring containers cookie dependent-sum
    dependent-sum-template ghcjs-dom HsOpenSSL http-client
    http-reverse-proxy http-types jsaddle jsaddle-warp lens modern-uri
    mtl network obelisk-asset-serve-snap obelisk-backend
    obelisk-executable-config-lookup obelisk-frontend obelisk-route
    process ref-tf reflex reflex-dom snap-core streaming-commons text
    time universe utf8-string wai wai-websockets warp warp-tls
    websockets which
  ];
  license = stdenv.lib.licenses.bsd3;
}
