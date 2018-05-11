module Main(main) where

import TPTPtoSMT.Problem
import TPTPtoSMT.Parse
import TPTPtoSMT.Pretty

main = do
  stream <- getContents
  case parseTPTP "<stdin>" stream of
    Left error -> print error
    Right tptp -> putStrLn $ pretty tptp
