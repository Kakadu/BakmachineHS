-- Interpreter of bytecode
module Main where

import Types

  

main = 
  do
    let x = IOutInt 
    
    putStrLn ("Hallo from Interpret " ++ (show x))
