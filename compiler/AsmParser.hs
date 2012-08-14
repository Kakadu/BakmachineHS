{-# LANGUAGE FlexibleContexts #-}
module AsmParser (
  inter, noop, program
) where

import Debug.Trace
import Text.Parsec hiding (label)
import Text.Parsec.String
import AsmTypes
import Data.Word

register :: Stream s m Char => ParsecT s () m RegisterSort
register =
  (h "ah" AH) <|> (h "bh" BH) <|> (h "ch" CH) <|> (h "dh" DH) <|> (h "eh" DH) 
  where h s r = string s >> return r

inter :: Stream s m Char => ParsecT s () m Statement
inter = 
  do
    string "int"
    spaces
    digits <- (many1 digit) <?> "need some digits"
    let i = trace (show digits) (read digits :: Word8)
    case inter_of_int i of
      Just x  -> return (trace (show ans) ans )
                 where ans = Interrupt x
      Nothing -> fail "no such interrupt"
      
int :: Stream s m Char => ParsecT s () m Word8
int = do lst <- many1 digit
         return $ (read lst :: Word8)
         
noop :: Stream s m Char => ParsecT s () m Statement
noop = string "nop" >> (return Nop)

pair :: Stream s m Char => ParsecT s () m a -> ParsecT s () m b -> (a -> b -> c) -> ParsecT s () m c
pair p1 p2 f =
  do l <- p1
     spaces
     r <- p2
     return $ (f l r)
     
mov :: Stream s m Char => ParsecT s () m Statement     
mov = 
  do 
    string "mov"
    spaces
    ans <- (pair int register Mov1) <|> (pair register register Mov2)
    return ans 

add :: Stream s m Char => ParsecT s () m Statement     
add = 
  do 
    string "add"
    spaces
    ans <- (pair int register Add1) <|> (pair register register Add2)
    return ans 

mul :: Stream s m Char => ParsecT s () m Statement     
mul = 
  do 
    string "mul"
    spaces
    ans <- register
    return $ Mul ans 

jumpHelper :: Stream s m Char => String -> (String -> Statement) -> ParsecT s () m Statement
jumpHelper s f = 
  do 
    string s 
    spaces
    ans <- many1 (letter <|> digit)
    return $ f ans
jg :: Stream s m Char => ParsecT s () m Statement         
jg = jumpHelper "jg" JumpGreater
je :: Stream s m Char => ParsecT s () m Statement         
je = jumpHelper "je" JumpEq
jl :: Stream s m Char => ParsecT s () m Statement         
jl = jumpHelper "jl" JumpLess


cmp :: Stream s m Char => ParsecT s () m Statement     
cmp = 
  do 
    string "cmp"
    spaces
    ans <- (pair register register Cmp2)
    return ans

label :: Stream s m Char => ParsecT s () m Statement
label = do
  ans <- many1 (letter <|> digit) 
  char ':'
  return $ Label ans
  
program :: Stream s m Char => ParsecT s () m [Statement]
program = 
  do ans <- many cmd
     eof
     return ans
  where cmd =             
          do 
            x <- inter <|> noop <|> (try mov) <|> cmp <|> add <|> mul <|> jg <|> je <|> jl <|> label
            --spaces
            char '\n' 
            return x
                 

