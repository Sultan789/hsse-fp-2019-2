module Parsing where
 
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

infixr 5 +++

instance Monad Parser where
   return v =  P (\inp -> [(v,inp)])
   p >>= f  =  P (\inp -> case parse p inp of
                             []        -> []
                             [(v,out)] -> parse (f v) out)

instance Applicative Parser where
   pure = return
   (<*>) = ap
   
instance Functor Parser where
   fmap = liftM

instance MonadPlus Parser where
   mzero        =  P (\inp -> [])
   p `mplus` q  =  P (\inp -> case parse p inp of
                                 []        -> parse q inp
                                 [(v,out)] -> [(v,out)])

instance Alternative Parser where
   empty = mzero
   (<|>) = mplus


newtype Parser a =  P (String -> [(a,String)])

failure :: Parser a
failure =  mzero

item :: Parser Char
item =  P (\inp -> case inp of
                      []     -> []
                      (x:xs) -> [(x,xs)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp  =  p inp



(+++) :: Parser a -> Parser a -> Parser a
p +++ q =  p `mplus` q


p :: Parser (Char, Char)
p = do x <- item
       item
       y <- item
       return (x,y)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if (p x) then 
             return x
           else 
             failure

char :: Char -> Parser Char
char c = sat (== c) 

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower =  sat isLower

upper :: Parser Char
upper =  sat isUpper

letter :: Parser Char
letter =  sat isAlpha

alphanum :: Parser Char
alphanum =  sat isAlphaNum

string :: String -> Parser String
string []      =  return []
string (x:xs)  =  do char x
                     string xs
                     return (x:xs)

many :: Parser a -> Parser [a]
many p =  many1 p +++ return []


many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)


ident :: Parser String
ident =  do x  <- lower
            xs <- many alphanum
            return (x:xs)


dList :: Parser String
dList  = do char '['
            d  <- digit
            ds <- many (do char ','
                           digit)
            char ']'
            return (d:ds)

nat :: Parser Int
nat =  do xs <- many1 digit
          return (read xs)

int :: Parser Int
int =  do char '-'
          n <- nat
          return (-n)
        +++ nat

space :: Parser ()
space =  do many (sat isSpace)
            return ()

token  :: Parser a -> Parser a
token p =  do space
              v <- p
              space
              return v

identifier  :: Parser String
identifier  =  token ident

natural :: Parser Int
natural =  token nat

integer :: Parser Int
integer =  token int

symbol :: String -> Parser String
symbol xs =  token (string xs)
