let pkgs =  import <nixpkgs> {};
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: {
        # transformers-ocharles = self.transformers;
        transformers-ocharles = self.callPackage ../transformers {};
      };
    };
in pkgs.stdenv.mkDerivation {
  name = "mtl-pipes-benchmark";
  src = ./.;
  buildInputs = [ (haskellPackages.ghcWithPackages (hs: [ hs.criterion hs.mtl hs.transformers-ocharles ])) ];
  buildPhase = ''
    ghc --make -O2 MtlPipes.hs -ddump-simpl -dsuppress-all > MtlPipes.hcr
  '';
  installPhase = ''
    mkdir -p $out
    cp MtlPipes $out/
    cp MtlPipes.hcr $out/
  '';
}
