module Types (
  RegisterSort (AH,BH,CH,DH,EH),
  InterruptSort (IExit, IOutInt),
  Bytecmd (Nop, Interrupt, Mov1, Mov2),
  inter_of_int
) where

-- types for bytecode commands
data RegisterSort = AH | BH | CH | DH | EH deriving (Show)

data InterruptSort = 
    IExit
  | IOutInt deriving (Show)

data Bytecmd =
    Mov1 Int RegisterSort 
  | Mov2 RegisterSort RegisterSort
  | Nop
  | Interrupt InterruptSort
  deriving (Show)


inter_of_int 10 = Just IExit
inter_of_int 11 = Just IOutInt
inter_of_int __ = Nothing
  