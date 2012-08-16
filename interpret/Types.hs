module Types where

import Data.Word

-- types for bytecode commands
data RegisterSort = AH | BH | CH | DH | EH | SP deriving (Show)

data InterruptSort = 
    IExit
  | IInputInt
  | IOutInt deriving (Show)

data Bytecmd =
    Mov1 Word8 RegisterSort 
  | Mov2 RegisterSort RegisterSort
  | Add1 RegisterSort RegisterSort
  | Sub1 RegisterSort RegisterSort
  | Add2 Word8 RegisterSort 
  | Mul RegisterSort
  | Cmp1 Word8 RegisterSort
  | Cmp2 RegisterSort RegisterSort
  | Nop
  | JumpGreater Word8
  | JumpEq      Word8
  | JumpLess    Word8
  | Interrupt InterruptSort
  deriving (Show)

instr_length :: Bytecmd -> Int
instr_length cmd = case cmd of
  Mov1 _ _ -> 3
  Mov2 _ _ -> 3
  Add1 _ _ -> 3
  Add2 _ _ -> 3
  Sub1 _ _ -> 3
  Mul   _  -> 2  
  Cmp1 _ _ -> 3
  Cmp2 _ _ -> 3
  Nop -> 1
  JumpGreater _ -> 2
  JumpLess _ -> 2
  JumpEq   _ -> 2
  Interrupt _ -> 2
  
code_of_reg :: RegisterSort -> Word8
code_of_reg x = case x of
  AH -> 0 
  BH -> 1
  CH -> 2
  DH -> 3
  EH -> 4
  SP -> 5

reg_of_code :: Word8 -> RegisterSort
reg_of_code 0 = AH  
reg_of_code 1 = BH
reg_of_code 2 = CH  
reg_of_code 3 = DH  
reg_of_code 4 = EH  
reg_of_code 5 = SP  
reg_of_code _ = error "Bad register code"

  
inter_of_int 10 = Just IExit
inter_of_int 11 = Just IOutInt
inter_of_int 13 = Just IInputInt
inter_of_int __ = Nothing
  
code_of_inter :: InterruptSort -> Word8
code_of_inter IExit = 10
code_of_inter IOutInt = 11
code_of_inter IInputInt = 13
