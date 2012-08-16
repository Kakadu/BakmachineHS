-- Interpreter of bytecode
module Main where

import Types
import qualified Data.ByteString.Lazy as BS hiding (putStrLn)
import Data.Word
import Debug.Trace
import Control.Applicative

readBytes :: String -> IO [Word8]
readBytes filename = BS.unpack <$> BS.readFile filename

buildProgram :: [Word8] -> [Maybe Bytecmd]
buildProgram xs =
  helper f xs
  where
    f xs = case xs of
      1 :x:r:tl -> Just (Mov1 x (wrap_reg r), tl)
      7 :l:r:tl -> Just (Mov2 (wrap_reg l) (wrap_reg r), tl)
      22:x:r:tl -> Just (Add2  x (wrap_reg r), tl)
      23:l:r:tl -> Just (Add1 (wrap_reg l) (wrap_reg r), tl)
      27:l:r:tl -> Just (Sub1 (wrap_reg l) (wrap_reg r), tl)
      31:r:tl   -> Just (Mul  (wrap_reg r), tl)
      50:x:r:tl -> Just (Cmp1 x (wrap_reg r), tl)
      51:l:r:tl -> Just (Cmp2 (wrap_reg l) (wrap_reg r), tl)
      20:icd:tl -> -- interrupt
        case inter_of_int icd of
          Just x  -> Just (Interrupt x,tl)
          Nothing -> error $ "No such interrupt: " ++ (show icd)
      46:l:tl  -> Just (JumpLess l,    tl)
      42:l:tl  -> Just (JumpEq   l,    tl)
      44:l:tl  -> Just (JumpGreater l, tl)
      []       -> Nothing
      xs       -> error $ "can't parse tail of input: " ++ (show xs)
    wrap_reg = reg_of_code

    helper _ [] = trace "Parsing finished." []
    helper f xs = case f xs of
        Just (cmd,tl) -> 
          l1 ++ (helper f tl)
          where len = instr_length cmd
                l1 :: [Maybe Bytecmd]
                l1 = (Just cmd) : map (const Nothing) [1..(len-1)]
        Nothing ->
          error $ "Can't parse whole program. " ++ (show xs)


data Env = Env {ah,bh,ch,dh,eh,sp::Word8}

--interpret :: [Bytecmd] -> IO Env
--interpret xs = 
--  exec_loop start
-- Prelude.foldl helper start
--  where 
--    miss_sp sp = error $ "No command on position " ++ (show sp)
--    exec_loop env =
--      env >>= \env -> case sp env of
--        sp | sp > length xs -> miss_sp xs
--        sp -> case xs !! sp of  
                
--    -helper env cmd = case cmd of
--      Interrupt IInputInt ->
--        env >>= (\env -> getLine >>= (\s -> return $ env {ah=read s::Word8}) )
--    start :: IO Env  
--    start = return (Env 0 0 0 0 0 0)

main = 
  do
    putStrLn "Hallo from Interpret "
    bytes <- readBytes "out.byte"
    let cmds = buildProgram bytes
    --putStrLn $ show cmds
    putStrLn $ concatMap (\x -> (show x) ++ "\n") cmds
    
        