module AsmTypes where

import Data.Word

-- types for bytecode commands
data RegisterSort = AH | BH | CH | DH | EH | SP deriving (Show)

data InterruptSort = 
    IExit
  | IInputInt
  | IOutInt deriving (Show)

data Statement =
    Add1 Word8 RegisterSort 
  | Add2 RegisterSort RegisterSort
  | Mov1 Word8 RegisterSort 
  | Mov2 RegisterSort RegisterSort
  | Mul RegisterSort 
  | Nop
  | Cmp2 RegisterSort RegisterSort
  | Interrupt InterruptSort
  | Label String
  | JumpGreater String
  | JumpEq      String
  | JumpLess    String
  deriving (Show)

code_of_reg :: RegisterSort -> Word8
code_of_reg x = case x of
  AH -> 0 
  BH -> 1
  CH -> 2
  DH -> 3
  EH -> 4
  SP -> 5

inter_of_int 10 = Just IExit
inter_of_int 11 = Just IOutInt
inter_of_int 13 = Just IInputInt
inter_of_int __ = Nothing
  
code_of_inter :: InterruptSort -> Word8
code_of_inter IExit = 10
code_of_inter IOutInt = 11
code_of_inter IInputInt = 13
