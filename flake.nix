{
  description = "Basic haskell flake";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.hicetnuncdao-docker =
       with import nixpkgs { system = "x86_64-linux"; };

       pkgs.dockerTools.buildImage {
         name = "hicetnuncdao";
         config = {
          Cmd = [ "${self.packages.x86_64-linux.hicetnuncdao}/bin/hicetnuncdao" ];
        };
      };
    defaultPackage.x86_64-linux = self.packages.x86_64-linux.hicetnuncdao;
    packages.x86_64-linux.hicetnuncdao =
       with import nixpkgs { system = "x86_64-linux"; };
       let
          inherit (pkgs.haskell.lib) appendPatch overrideCabal addExtraLibrary addBuildDepends;
          haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: rec {
              obelisk-executable-config-inject = self.callPackage ./obelisk/obelisk-executable-config-inject.nix {};
              obelisk-executable-config-lookup = self.callPackage ./obelisk/obelisk-executable-config-lookup.nix {};
              snap-core = appendPatch super.snap-core ./obelisk/patches/check-range.patch;
              obelisk-asset-serve-snap = appendPatch (self.callPackage ./obelisk/obelisk-asset-serve-snap.nix { snap-core = self.snap-core; }) ./obelisk/patches/remove-data-monoid-import.patch;
              obelisk-tabulation = self.callPackage ./obelisk/obelisk-tabulation.nix {};
              obelisk-snap-extras = self.callPackage ./obelisk/obelisk-snap-extras.nix {};
              obelisk-backend = self.callPackage ./obelisk/obelisk-backend.nix {};
            };
          };
          haskellDeps = ps: with ps; [
            reflex-dom
            categories
            cookie
            either
            ref-tf
            snap
            snap-server
            modern-uri
            websockets
            warp
            warp-tls
            http-reverse-proxy
            jsaddle
            jsaddle-warp
            monad-control
            universe
            universe-dependent-sum
            obelisk-executable-config-inject
            obelisk-executable-config-lookup
            obelisk-tabulation
            universe-some
            obelisk-snap-extras
            obelisk-asset-serve-snap
            clay
            ghcide
            ghcid
          ];
          ghc = haskellPackages.ghcWithHoogle haskellDeps;
       in 
       stdenv.mkDerivation {
          pname = "hicetnuncdao";
          version = "0.2";
          buildInputs = [ ghc pkgs.ghcid ];
          src = self;
          buildPhase = "ghc -o $pname Main.hs";
          installPhase = "mkdir -p $out/bin; install -t $out/bin $pname";
    };
  };
}
