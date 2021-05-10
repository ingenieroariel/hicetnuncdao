# hicetnuncdao.xtz

Hic et Nunc DAO (hDAO) community site.

# developing
nix develop
ghcid -r Main.hs

# building (a frontend / backend combined binary)
nix build

# deploying (container can then run on any container framework, like Kubernetes)
nix build #.hicetnuncdao-docker
docker load < ./result
docker tag hicetnuncdao:hash piensa/hicetnuncdao:hash
docker push piensa/hicetnuncdao:hash

# credits
This repo uses a non standard version of Obelisk ( https://github.com/obsidiansystems/obelisk ) a really awesome functional reactive framework for web and mobile applications, with batteries included.
