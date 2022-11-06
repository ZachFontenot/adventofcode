{ sources ? import ./nix/sources.nix, compiler ? "ghc92" }:

let
  pkgs = import sources.nixpkgs { };

in
  pkgs.mkShell {
    nativeBuildInputs = with pkgs; [
      hpack
      haskell.compiler.${compiler}
      #haskell.packages.${compiler}.ghcid
      haskell.packages.${compiler}.terminal-size
      haskellPackages.implicit-hie
      ghc
      ghcid
      cabal-install
      ormolu
      zlib
      haskell.packages.${compiler}.haskell-language-server
    ];
  }
