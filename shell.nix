with import <nixpkgs> {};

let
  haskellDeps = ps: [
    ps.megaparsec
    ps.haskell-language-server
    ps.hlint
  ];
  ghc = unstable.haskellPackages.ghcWithPackages haskellDeps;
  nixPackages = [
    ghc
    binutils
    ];
in
mkShell rec {
  buildInputs = nixPackages;
}
