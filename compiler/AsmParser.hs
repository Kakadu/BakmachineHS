{-# LANGUAGE FlexibleContexts #-}
module AsmParser (
  inter, noop, program
) where

import Debug.Trace
import Text.Parsec
import Text.Parsec.String
import Types
import Data.Word

register :: Stream s m Char => ParsecT s () m RegisterSort
register =
  (h "ah" AH) <|> (h "bh" BH) <|> (h "ch" CH) <|> (h "dh" DH) <|> (h "eh" DH) 
  where h s r = string s >> return r

inter :: Stream s m Char => ParsecT s () m Bytecmd
inter = 
  do
    string "int"
    spaces
    digits <- (many1 digit) <?> "need some digits"
    let i = trace (show digits) (read digits :: Word8)
    case Types.inter_of_int i of
      Just x  -> return (trace (show ans) ans )
                 where ans = Interrupt x
      Nothing -> fail "no such interrupt"
      
int :: Stream s m Char => ParsecT s () m Word8
int = do lst <- many1 digit
         return $ (read lst :: Word8)
         
noop :: Stream s m Char => ParsecT s () m Bytecmd
noop = string "nop" >> (return Nop)

pair :: Stream s m Char => ParsecT s () m a -> ParsecT s () m b -> (a -> b -> c) -> ParsecT s () m c
pair p1 p2 f =
  do l <- p1
     spaces
     r <- p2
     return $ (f l r)
     
mov :: Stream s m Char => ParsecT s () m Bytecmd     
mov = 
  do 
    string "mov"
    spaces
    ans <- (pair int register Mov1) <|> (pair register register Mov2)
    return ans 

cmp :: Stream s m Char => ParsecT s () m Bytecmd     
cmp = 
  do 
    string "cmp"
    spaces
    ans <- (pair register register Cmp2)
    return ans

program :: Stream s m Char => ParsecT s () m [Bytecmd]
program = 
  do ans <- many cmd
     eof
     return ans
  where cmd =             
          do 
            x <- inter <|> noop <|> mov <|> cmp
            --spaces
            char '\n' 
            return x
                 

