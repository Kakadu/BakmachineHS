module Main where 
-- compiler of bytecode

import Text.Parsec
import AsmParser
import qualified GenByte
import qualified Numeric

import Data.Binary.Put
import Data.ByteString as ByteString hiding (putStrLn,  readFile, putStr, map)
import Data.Word

serializeProgram :: [Word8] -> IO ()
serializeProgram xs = 
  ByteString.writeFile "out.byte" $ Prelude.foldl ByteString.snoc ByteString.empty xs

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
          putStrLn ("bytes: " ++ (show $ map (\x -> Numeric.showHex x "") ints ) )
          serializeProgram ints
    
    
