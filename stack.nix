with (import <nixpkgs> {});
let
  haskellPackages = haskell.packages.lts-6_7;
  ghc = haskellPackages;
in
haskell.lib.buildStackProject {
  name = "myEnv";
#    buildInputs = [ gcc git zlib pkgconfig ghc glibcLocales ];
  buildInputs = [ zlib haskellPackages.ghc-mod ];
  ghc = haskellPackages.ghc;
}
