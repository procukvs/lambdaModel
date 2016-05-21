module Parser (parseTerm) where

import Data.Char
import Data.List
import Text.Parsec
--import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Syntax
--------------------------------------------------- 
-- lambda = "\" id {id} "." term 
-- fact   = "(" term ")" | id | lambda | number
-- app    = fact {fact}
-- let    = "let" id "=" term "in" term
-- term   = let | app 
-- decl   = "let" id = term ";" 
-- prog   = { decl} term "eof"
---------------------------------------------------

type LCParser = Parsec String Contex 

lexer :: Tok.TokenParser Contex
lexer = Tok.makeTokenParser style
  where ops = ["->","\\","+","*","-","="]
        names = ["let", "in"]
        style = haskellStyle {Tok.reservedOpNames = ops,
                              Tok.reservedNames = names,
                              Tok.commentLine = "--"}

reserved :: String -> LCParser ()
reserved = Tok.reserved lexer

reservedOp :: String -> LCParser ()
reservedOp = Tok.reservedOp lexer

identifier :: LCParser String
identifier = Tok.identifier lexer 

natural :: LCParser Integer
natural = Tok.natural lexer

semi :: LCParser String
semi = Tok.semi lexer

parens :: LCParser a -> LCParser a
parens = Tok.parens lexer

contents :: LCParser a -> LCParser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r
  

findIndexP :: String -> Contex -> Maybe Int 
findIndexP nm ctx = find nm ctx 0 
  where find n [] i = Nothing 
        find n ((id,_):ct) i | (id==n) = (Just i)
        find n (_:ct) i = find n ct (i+1)

--variable :: Parser Term
variable :: LCParser Term
variable = do
  x <- identifier
  cnt <- getState 
  case findIndexP x cnt of 
     Nothing -> fail ("unbounded variable: " ++ x)  
--     Nothing -> Left ("unbounded variable: " ++ x)  
     Just n  -> return (Var x n (length cnt))
 {-
integerTerm :: Integer -> Int -> Term
integerTerm n lc = (Abs "s" (Abs "z" (buildTerm n))) 
  where buildTerm 0 = Var "z" 0 (lc+2) 
        buildTerm n = (App (Var "s" 1 (lc+2)) (buildTerm (n-1)))  
-}
number :: LCParser Term
number = do
  n <- natural
  cnt <- getState
  return (Nmb (show n) (fromIntegral n))  
  -- return (integerTerm n (length cnt))

--lambda :: Parser Term
lambda :: LCParser Term
lambda = do
  reservedOp "\\"
  cnt <- getState
  args <- many1 identifier
  let binding = map (\x-> (x,NmBind))(reverse args)  
  putState (binding ++ cnt) 
  reservedOp "."
  body <- term
  putState cnt
  return $ foldr Abs body args

letterm :: LCParser Term
letterm = do
  reserved "let"
  x <- identifier
  reservedOp "=" 
  def <- term
  reserved "in"  
  cnt <- getState
  putState ((x,(TmBind def)) : cnt) 
  body <- term
  putState cnt
  return (Let x def body)  

fact :: LCParser Term
fact =  parens term
    <|> variable
    <|> number
    <|> lambda

app :: LCParser Term
app = do
  es <- many1 fact
  return (foldl1 App es)

term :: LCParser Term
term = letterm 
     <|> app

decl :: LCParser ()
decl = do
  reserved "let"
  x <- identifier
  cnt <- getState
  args <- many identifier
  let binding = map (\x-> (x,NmBind))(reverse args)  
  putState (binding ++ cnt)   
  reservedOp "=" 
  def <- term
  semi
  putState ((x,(TmBind (foldr Abs def args))):cnt)
  return () 

             --  putState ((x,(TmBind def)) : cnt)
  
program :: LCParser Program 
program = do 
  many decl 
  t <- term 
  cnt <- getState 
  return (Program cnt t)
  
  
parseTerm :: String ->  Either ParseError Program
parseTerm input = runParser (contents program)  [] "untyped lambda model" input  
{-  
parseTerm :: String -> Either ParseError Term
parseTerm input = parse (contents term) "<stdin>" input
-}