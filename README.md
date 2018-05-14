# A translator from TPTP to SMT-LIB 2

This is a simple translator that I used to convert some problems that I had from the TPTP TFF syntax to SMT-LIB 2. The translator only supports the features of TPTP that I needed and is not meant to work for all kinds of problems.

I am not aware of a general purpose TPTP to SMT-LIB translator ([TPTP2X and TPTP4X](http://www.cs.miami.edu/~tptp/Seminars/TPTP/TPTPNX.html) haven't been updated for a long while). If you would like to write one, feel free to use this code as a start.

Run `cabal install` to compile and install. You will need [Haskell Platform](https://www.haskell.org/platform/). Then run `tptp2smt`, passing the TPTP code in the standard input.