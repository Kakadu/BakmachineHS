module GenByte (
  gen_program
) where

import AsmTypes
import Data.Word

gen_cmd :: Bytecmd -> [Word8]
gen_cmd cmd = 
  case cmd of 
    Mov2 x r -> [1, code_of_reg x, code_of_reg r]
    Mov1 x r -> [7, x            , code_of_reg r]
    Nop      -> [19]
    Interrupt i -> [20, code_of_inter i]

gen_program :: [Bytecmd] -> [Word8]
gen_program = concatMap gen_cmd

