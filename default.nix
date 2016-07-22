{ mkDerivation, array, base, containers, mtl, stdenv, transformers
, xhb, xhb-keysyms, xhb-monad
}:
mkDerivation {
  pname = "xhb-mapping-state";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    array base containers mtl transformers xhb xhb-keysyms xhb-monad
  ];
  license = stdenv.lib.licenses.mit;
}
