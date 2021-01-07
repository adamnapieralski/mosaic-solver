# mosaic-solver
Haskell program for solving a mosaic ([fill-a-pix](https://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix)) puzzle.

## Prerequisites
- [Glashow Haskell Compiler](https://www.haskell.org/ghc/)
- [Cabal build system](https://www.haskell.org/cabal/)

Can be installed within [Haskell Platform](https://www.haskell.org/platform/) installation.

## Build
To build executable file run:
```
cabal build
```
It creates a new `dist/` directory with all build output files.
## Run
Built executable file can be found in `dist/build/mosaic-solver/mosaic-solver`. It can be then run (on Linux) with:
```
./dist/build/mosaic-solver/mosaic-solver
```
But you can simply build and immediately run the program using:
```
cabal run
```

## Test data
Sample input data is provided in `data/` directory.

## Showcase
You can see mosaic-solver in work with showcase data inputs running:
```
./showcase.sh
```
Its results are also presented in [showcase_out.txt](./showcase_out.txt).

## Authors
- Adam Napieralski - [adamnapieralski](https://github.com/adamnapieralski)
- Łukasz Kostrzewa - [kost13](https://github.com/kost13)
