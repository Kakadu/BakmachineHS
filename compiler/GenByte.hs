module GenByte (
  gen_program
) where

import Types

gen_cmd :: Bytecmd -> [Int]
gen_cmd cmd = 
  case cmd of 
    Mov2 x r -> [1, code_of_reg x, code_of_reg r]
    Mov1 x r -> [7, x            , code_of_reg r]
    Nop      -> [19]
    Interrupt i -> [20, code_of_inter i]

gen_program :: [Bytecmd] -> [Int]
gen_program = concatMap gen_cmd
