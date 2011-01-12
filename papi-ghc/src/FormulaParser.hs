module FormulaParser(
    parse
  , run
)
where

import Formula
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language


{----------------------------------------------------------
 - Lexing
 ---------------------------------------------------------}
lexer :: T.TokenParser () 
lexer = T.makeTokenParser (
          emptyDef { 
              identStart      = letter 
            , identLetter     = alphaNum <|> oneOf "_:."
            , reservedOpNames = ["*","/","+","-", "**"] 
          }
        )

naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat    = T.naturalOrFloat lexer

parens :: Parser Expr -> Parser Expr
parens     = T.parens lexer

identifier :: Parser String
identifier = T.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

expr :: Parser Expr
expr =  buildExpressionParser table term
        <?> "expression"

{----------------------------------------------------------
 - Parsing
 ---------------------------------------------------------}
table	:: OperatorTable Char () Expr
table	= [ [prefix "-" negate]
        , [binary "**" (**) AssocLeft]
        , [binary "*"  (*) AssocLeft, binary "/" (/) AssocLeft ] 
        , [binary "+"  (+) AssocLeft, binary "-" (-)	AssocLeft ] ]
  where
  prefix name fun	= Prefix (do{ reservedOp name; return $ Uop name fun }) 
  binary name fun assoc = Infix (do{ reservedOp name; return $ Bop name fun }) assoc 

term :: Parser Expr
term =     parens expr
       <|> (naturalOrFloat    >>= (return . Const . toConst))
       <|> (identifier >>= (return . Var))
       <?> "simple expression"

toConst :: Either Integer Double -> Double
toConst (Left i)  = fromIntegral i
toConst (Right d) = d

formula :: Parser Formula
formula = do
  whiteSpace
  f <- identifier
  whiteSpace
  _ <- char '='
  whiteSpace
  x <- expr
  eof
  return $ Formula f x

parse :: FilePath -> String -> Either ParseError Formula
parse fileName input = P.parse formula fileName input

run :: String -> IO () 
run input = 
  case (P.parse formula "" input) of 
    Left err -> do{ putStr "parse error at " ; print err } 
    Right x -> print x

