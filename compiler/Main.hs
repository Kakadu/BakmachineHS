module Main where 
-- compiler of bytecode

import Text.Parsec
import AsmParser

main = 
  do
    putStrLn "Hallo world from compiler"
    input <- readFile "input"
    case runParser AsmParser.program () "" input of
      Right result  -> putStrLn ("right: " ++ show result)
      Left err -> 
        do{ putStr "parse error at "
          ; print err
          }
    
    
