-- Interpreter of bytecode
module Main where

import Types
import Data.ByteString.Lazy as BS hiding (putStrLn)
import Data.Word
import Control.Applicative

readBytes :: String -> IO [Word8]
readBytes filename = BS.unpack <$> BS.readFile filename

buildProgram :: [Word8] -> [Bytecmd]
buildProgram xs =
  case xs of
	  1 :x:r:tl -> Mov1 x (wrap_reg r) : buildProgram tl
	  7 :l:r:tl -> Mov2 (wrap_reg l) (wrap_reg r) : buildProgram tl
	  22:x:r:tl -> Add2  x (wrap_reg r) : buildProgram tl
	  23:l:r:tl -> Add1 (wrap_reg l) (wrap_reg r) : buildProgram tl
	  27:l:r:tl -> Sub1 (wrap_reg l) (wrap_reg r) : buildProgram tl
	  31:r:tl   -> Mul (wrap_reg r) : buildProgram tl
	  50:x:r:tl -> Cmp1 x (wrap_reg r) : buildProgram tl
	  51:l:r:tl -> Cmp2 (wrap_reg l) (wrap_reg r) : buildProgram tl
	  20:icd:tl -> -- interrupt
		case inter_of_int icd of
		  Just x -> (Interrupt x) : buildProgram tl
		  Nothing -> error $ "No such interrupt: " ++ (show icd)
	  46:l:tl  -> JumpLess l    : buildProgram tl
	  42:l:tl  -> JumpEq   l    : buildProgram tl
	  44:l:tl  -> JumpGreater l : buildProgram tl
	  []       -> []
	  xs       -> error $ "can't parse tail of input: " ++ (show xs)
  where 
    wrap_reg = reg_of_code
	
main = 
  do
    putStrLn "Hallo from Interpret "
    bytes <- readBytes "out.byte"
    let cmds = buildProgram bytes
    putStrLn $ show cmds
		