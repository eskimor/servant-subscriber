{ nixpkgs ? import <nixpkgs> {}, ghc }:
    let
        buildStackProject = nixpkgs.haskell.lib.buildStackProject.override { inherit ghc;};
    in
        with nixpkgs;

        buildStackProject {
            name = builtins.trace ghc.name "servant-subscriber-env";
            buildInputs = [ gcc git zlib ];
            shellHook = "export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt";
        }
