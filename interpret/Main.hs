-- Interpreter of bytecode
module Main where

import Types
import qualified Data.ByteString.Lazy as BS hiding (putStrLn)
import Data.Int
import Data.Word
import Debug.Trace
import Control.Applicative

readBytes :: String -> IO [Word8]
readBytes filename = BS.unpack <$> BS.readFile filename

buildProgram :: [Word8] -> [Maybe Bytecmd]
buildProgram xs =
  helper f xs
  where
    f :: [Word8] -> Maybe (Bytecmd, [Word8])
    f xs = case xs of
      1 :x:r:tl -> Just (Mov1 (fromIntegral x :: Int16) (wrap_reg r), tl)
      7 :l:r:tl -> Just (Mov2 (wrap_reg l) (wrap_reg r), tl)
      22:x:r:tl -> Just (Add2 (fromIntegral x :: Int16) (wrap_reg r), tl)
      23:l:r:tl -> Just (Add1 (wrap_reg l) (wrap_reg r), tl)
      27:l:r:tl -> Just (Sub1 (wrap_reg l) (wrap_reg r), tl)
      31:r:tl   -> Just (Mul  (wrap_reg r), tl)
      50:x:r:tl -> Just (Cmp1 (fromIntegral x :: Int16) (wrap_reg r), tl)
      51:l:r:tl -> Just (Cmp2 (wrap_reg l) (wrap_reg r), tl)
      20:icd:tl -> -- interrupt
        case inter_of_int icd of
          Just x  -> Just (Interrupt x,tl)
          Nothing -> error $ "No such interrupt: " ++ (show icd)
      46:l:tl  -> Just (JumpLess    (fromIntegral l :: Int16), tl)
      42:l:tl  -> Just (JumpEq      (fromIntegral l :: Int16), tl)
      44:l:tl  -> Just (JumpGreater (fromIntegral l :: Int16), tl)
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


data Env = Env {ah,bh,ch,dh,eh,sp::Int16}
instance Show Env where
  show (Env ah bh ch dh eh sp) = 
    "Env {ah=" ++ show ah ++ ", bh=" ++ show bh ++ ", ch=" ++ show ch ++ ", dh=" ++ show dh ++ ", eh=" ++ show eh ++ ", sp=" ++ show sp ++ "}"

access_reg :: Env -> RegisterSort -> Int16
access_reg env r = case r of
  AH -> ah env
  BH -> bh env
  CH -> ch env
  DH -> dh env
  EH -> eh env
  SP -> sp env
  
data NextCmd = Next | Jump Int16 | TheEnd

interpret :: [Maybe Bytecmd] -> IO Env
interpret xs = 
  exec_loop start
  where 
    miss_sp sp = error $ "No command on position " ++ (show sp)
    exec_loop :: IO Env -> IO Env
    exec_loop env =
      env >>= \env -> case sp env of
        sp | fromIntegral sp > length xs -> miss_sp xs
        cur_sp -> 
          case xs !! (fromIntegral cur_sp) of  
            Nothing -> error $ "can't execute middle of some command"
            Just cmd -> case helper env cmd of
              (env,Jump addr) -> exec_loop $ env >>= \env -> return$ env {sp = addr}
              (env,Next)      -> 
                exec_loop $ env >>= \env -> return env {sp = cur_sp + (instr_length cmd)}
              (env,TheEnd)    -> env

    helper :: Env -> Bytecmd -> (IO Env, NextCmd)
    helper env cmd = 
      --trace (show env) $ trace (show cmd) $ 
      case cmd of
        Interrupt IInputInt ->
         (putStrLn "Enter a number" >> getLine >>= (\s -> return $ env {ah=read s::Int16}),Next)
        Interrupt IOutInt ->
          (putStrLn (show $ ah env) >> (return env), Next)
        Interrupt IExit -> (return env, TheEnd)
        JumpEq      loc | eh env == 0 -> (return env, Jump loc)
        JumpEq      ____              -> (return env, Next)
        JumpLess    loc | eh env <  0 -> (return env, Jump loc)
        JumpLess    ____              -> (return env, Next)
        JumpGreater loc | eh env >  0 -> (return env, Jump loc)
        JumpGreater ____              -> (return env, Next)
        Nop      -> (return env, Next)
        Mov1 x r -> (return $ put_reg env r (const x), Next)
        Mov2 x r -> (return $ put_reg env r (const $ access_reg env x), Next)
        Add1 x r -> (return $ put_reg env r ((+)(access_reg env x)), Next)
        Sub1 x r -> (return $ put_reg env r (\r -> r - (access_reg env x)), Next)
        Add2 x r -> (return $ put_reg env r ((+)x), Next)
        Mul r    -> (return $ put_reg env AH ((*)(access_reg env r)), Next)
        Cmp1 x r -> (return $ put_reg env EH (const $ f (access_reg env r)), Next)
          where f r | x==r = 0
                f r | x<r  = -1
                f r | x>r  = 1
        Cmp2 l r -> (return $ put_reg env EH (const $ f (access_reg env l) (access_reg env r)), Next)
          where f l r | l==r = 0
                f l r | l<r  = -1
                f l r | l>r  = 1

    val_of_reg = access_reg
    put_reg :: Env -> RegisterSort -> (Int16 -> Int16) -> Env
    put_reg env r f = case r of
      AH -> env {ah = f $ ah env}
      BH -> env {bh = f $ bh env}
      CH -> env {ch = f $ ch env}
      DH -> env {dh = f $ dh env}
      EH -> env {eh = f $ eh env}
      SP -> env {sp = f $ sp env}
    start :: IO Env
    start = return (Env 0 0 0 0 0 0)

main = 
  do
    putStrLn "Hallo from Interpret "
    bytes <- readBytes "out.byte"
    let cmds = buildProgram bytes
    putStrLn $ concatMap (\x -> (show x) ++ "\n") cmds
    interpret cmds
        