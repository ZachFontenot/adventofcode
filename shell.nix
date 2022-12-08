let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  ghc_version = "ghc92";
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    # Beam things
    elixir_1_14
    erlangR25
    rebar3

    # Forth
    pforth
  
    # Haskell things
    hpack
    haskell.compiler.${ghc_version}
    #haskell.packages.${ghc_version}.ghcid
    haskell.packages.${ghc_version}.terminal-size
    haskellPackages.implicit-hie
    ghc
    ghcid
    cabal-install
    ormolu
    haskell.packages.${ghc_version}.haskell-language-server

    #Racket is already installed :(

    ### MISC
    zlib
  ];
}
