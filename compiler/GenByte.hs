module GenByte where

import AsmTypes
import Data.Word
import qualified Data.Map.Lazy as Map

gen_program :: [Statement] -> [Word8]
gen_program xs = 
  let (labelMap,_) = foldl helper (Map.empty,0) xs
      helper (acc,i) (Label s) = (Map.insert s i acc,i)
      helper (acc,i) x         = (acc, i + lengther x) 
      jumpHelper pos (JumpLess x)    = helper' 46 x
      jumpHelper pos (JumpEq x)      = helper' 42 x
      jumpHelper pos (JumpGreater x) = helper' 44 x
      jumpHelper pos _  = error "asdfasfqwefp qerv"
      helper' code s = 
        case Map.lookup s labelMap of
          Just x -> [code, x]
          Nothing -> error "no such label"
      gen_cmd (i,cmd) =
        case cmd of
          Mov2 x r   -> [7,  code_of_reg x, code_of_reg r]
          Mov1 x r   -> [1,  x            , code_of_reg r]
          Add1 x r   -> [22, x            , code_of_reg r]
          Add2 r1 r2 -> [23, code_of_reg r1, code_of_reg r2]
          Mul r      -> [31, code_of_reg r]
          Nop        -> [19]
          Cmp2 r1 r2 -> [51, code_of_reg r1, code_of_reg r2]		  
          Interrupt i   -> [20, code_of_inter i]
          Label _       -> []
          JumpGreater _ -> jumpHelper i cmd
          JumpEq _      -> jumpHelper i cmd
          JumpLess _    -> jumpHelper i cmd
      in
  concatMap gen_cmd $ zip [1..] xs

