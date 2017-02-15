with (import <nixpkgs> {});
let
  ghc = haskellPackages.ghc;
in
haskell.lib.buildStackProject {
  name = "myEnv";
#    buildInputs = [ gcc git zlib pkgconfig ghc glibcLocales ];
  buildInputs = [ zlib ];
  ghc = ghc;
}
