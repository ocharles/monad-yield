{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "transformers";
  version = "0.5.1.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  description = "Concrete functor and monad transformers";
  license = stdenv.lib.licenses.bsd3;
}
