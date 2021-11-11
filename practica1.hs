{

module Practica1 where
import Data.Char

}
%name parser
%tokentype { Token }
%error { parseError }

%token 
      let             { TokenLet}
      letrec          { TokenLetrec }
      var             { TokenVar $$ }
      num             { TokenNum $$ }
      '='             { TokenEq }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenOB }
      ')'             { TokenCB }
      

%%

Exp  : let var '=' Exp in Exp     { Let $2 $4 $6 }
     | letrec var '=' Exp in Exp  { letrec $2 $4 $6 }
     | Exp1                       {Exp1 $1 }

Exp1  : Exp1 '+' Term           { Plus $1 $3 } 
     | Exp1 '-' Term           { Minus $1 $3 }
     | Term                    { Term $1 }  

Term  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

Factor			  
      : num                     { Num $1 }
      |var                      { Var $1 }
      |'(' Exp ')'             { Brack $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
      = Let String Exp Exp
      | Letrec String Exp Exp
      | Exp1 Exp1
      deriving Show
data Exp1 
      = Plus Exp1 Term 
      | Minus Exp1 Term 
      | Term Term
      deriving Show

data Term 
      = Times Term Factor 
      | Div Term Factor 
      | Factor Factor
      deriving Show

data Factor 
      = Num Int 
      | Var String
      | Brack Exp
      deriving Show


data Token
      = TokenLet
      | TokenLetrec 
      | TokenVar String
      | TokenNum Int
      | TokenEq
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenOB
      | TokenCB
      deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs

lexNum cs = TokenNum (read num) : lexer rest
      where (num,rest) = span isDigit cs


main = getContents >>= print . parser . lexer

}