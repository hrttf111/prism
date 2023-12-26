{ callCabal2nix, haskell, cabal-install, hpack, zlib1, yasm, gdb, python3 }:

with haskell.lib;

let
  prismOrigin = justStaticExecutables (callCabal2nix "prism" ./. {});
in
  (overrideCabal prismOrigin (old: {
    pname = "prism";
    buildDepends = [ cabal-install hpack zlib1 yasm gdb python3 ];
  })).overrideAttrs (finalAttrs: previousAttrs: {
    #propagatedBuildInputs = [ zlib1 ];
    LD_LIBRARY_PATH = "${zlib1}/lib:$LD_LIBRARY_PATH";
    shellHook = ''
      alias test-cabal="cabal test --test-show-details=direct"
    '';
  })
