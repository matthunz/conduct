stack exec ormolu -- --mode inplace $(find . -name '*.hs')
cargo fmt
cabal format
