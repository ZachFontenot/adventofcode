with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "haskell-shell";
  buildInputs = with pkgs.haskellPackages;
    [ pkgs.zlib
      cabal-install
      ghcid
      hlint
      ormolu
      ghc
    ];
  }
