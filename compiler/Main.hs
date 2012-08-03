module Main where 
-- compiler of bytecode

import Text.Parsec
import AsmParser
import qualified GenByte

import Data.Binary.Put
--import Data.ByteString.Lazy

--serializeProgram :: [Int] -> Put
--serializeProgram xs = 
  
--  where loop acc [] = acc
--        loop acc x:xs = 
--          case x of
            


main = 
  do
    putStrLn "Hallo world from compiler"
    input <- readFile "input"
    case runParser AsmParser.program () "" input of
      Left err -> 
        do{ putStr "parse error at "
          ; print err
          }
      Right result  -> 
        do 
          putStrLn ("right: " ++ show result)
          let ints = GenByte.gen_program result 
          putStrLn ("bytes: " ++ show ints)
    
    
