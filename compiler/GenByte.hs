module GenByte where

import AsmTypes
import Data.Word
import qualified Data.Map.Lazy as Map

gen_program :: [Statement] -> [Word8]
gen_program xs = 
  let labelMap = foldl helper Map.empty xs
      helper (acc,i) (Label s) = (Map.insert s i acc,i)
      helper (acc,i) x         = (acc, i + lengther x) 
      jumpHelper pos (JumpLess x)    = helper' 46 x
      jumpHelper pos (JumpEq x)      = helper' 42 x
      jumpHelper pos (JumpGreater x) = helper' 44 x
      jumpHelper pos _  = error "asdfasfqwefp qerv"
      helper' code s = 
        case Map.lookup s labelMap of
          Just x -> [code, x]
          Nothing -> "error: no such label"
      gen_cmd (i,cmd) =
        case cmd of
          Mov2 x r -> [1, code_of_reg x, code_of_reg r]
          Mov1 x r -> [7, x            , code_of_reg r]
          Nop      -> [19]
          Interrupt i -> [20, code_of_inter i]
   
  concatMap gen_cmd $ zip [1..] xs

