REVISION="v0.8.0.0"

cabal2nix https://github.com/obsidiansystems/obelisk --revision $REVISION --subpath lib/executable-config/inject >> obelisk-executable-config-inject.nix
cabal2nix https://github.com/obsidiansystems/obelisk --revision $REVISION --subpath lib/executable-config/lookup >> obelisk-executable-config-lookup.nix
cabal2nix https://github.com/obsidiansystems/obelisk --revision $REVISION --subpath lib/route >> obelisk-route.nix

cabal2nix https://github.com/obsidiansystems/obelisk --revision $REVISION --subpath lib/run >> obelisk-run.nix


cabal2nix https://github.com/obsidiansystems/obelisk --revision $REVISION --subpath lib/tabulation >> obelisk-tabulation.nix


cabal2nix https://github.com/obsidiansystems/obelisk --revision $REVISION --subpath lib/backend >> obelisk-backend.nix
cabal2nix https://github.com/obsidiansystems/obelisk --revision $REVISION --subpath lib/frontend >> obelisk-frontend.nix

cabal2nix https://github.com/obsidiansystems/obelisk --revision $REVISION --subpath lib/asset/serve-snap >> obelisk-asset-serve-snap.nix

cabal2nix https://github.com/obsidiansystems/obelisk --revision $REVISION --subpath lib/snap-extras >> obelisk-snap-extras.nix
