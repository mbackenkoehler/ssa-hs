{-# LANGUAGE TupleSections   #-}
------------------------------------------------------------------------------
{-|
  Module      : SSA.Parser
  Description : Model parser
  Copyright   : 2016 Michael Backenköhler
  License     : MIT
  Maintainer  : Michael Backenköhler
  Stability   : experimental
  Portability : portable
-}
------------------------------------------------------------------------------
module SSA.Parser
      ( parseModel
      ) where
------------------------------------------------------------------------------
import qualified Data.Bifunctor as B
import           Text.Parsec hiding ( State )
import           Text.Parsec.Expr
import           Text.Parsec.String
import           Text.Parsec.Token
import           Text.Parsec.Language
------------------------------------------------------------------------------
import           SSA.Model
import           SSA.Expr
------------------------------------------------------------------------------
TokenParser
  { parens = m_parens
  , identifier = m_identifier
  , reservedOp = m_reservedOp
  , reserved = m_reserved
  , semiSep1 = m_semiSep1
  , whiteSpace = m_whiteSpace
  , float = m_float
  , integer = m_integer
  , natural = m_natural
  } = makeTokenParser $ emptyDef
  { commentLine = "--"
  , identStart = letter
  , identLetter = alphaNum
  , opStart = oneOf "+-*/@;:"
  , opLetter = oneOf "+>-*/@;:"
  , reservedOpNames = ["+","-","*","/","@",";",":","->"]
  , reservedNames = ["parameters","species","int","bool","init","reactions"]
  }

expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"
  where
    table = [ [ Infix (m_reservedOp "*" >> return (BinOp Mul)) AssocLeft
              , Infix (m_reservedOp "/" >> return (BinOp Div)) AssocLeft]
            , [ Infix (m_reservedOp "+" >> return (BinOp Add)) AssocLeft
              , Infix (m_reservedOp "-" >> return (BinOp Sub)) AssocLeft]
            ]

term :: Parser Expr
term = m_parens expr
   <|> fmap (Atomic . Var) m_identifier
   <|> fmap (Atomic . Const) num

num :: Parser Double
num = try m_float <|> fmap fromIntegral m_integer

parameterList :: Parser [(Ident, Double)]
parameterList = m_reserved "parameters" >> many parameter <?> "parameters"

parameter :: Parser (Ident, Double)
parameter = (,) <$> m_identifier <*> (m_reservedOp "=" >> num)

speciesList :: Parser [Species]
speciesList = m_reserved "species" >> many specie <?> "species"

specie :: Parser Species
specie = flip Species <$> m_identifier <*> (m_reservedOp ":" >> typ)

typ :: Parser SpeciesType
typ = (m_reserved "int" >> pure HCopy) <|> (m_reserved "bool" >> pure LCopy)

reactsList :: Parser [PReaction]
reactsList = m_reserved "reactions" >> many reaction <?> "reactions"

reaction :: Parser PReaction
reaction = do
  input <- reactants
  m_reservedOp "->"
  output <- reactants
  m_reservedOp "@"
  rate <- expr
  m_reservedOp ";"
  return (PReaction input output rate)

reactants :: Parser [(Ident, Nat)]
reactants = none <|> reactant `sepBy` m_reservedOp "+" <?> "reactants"
  where none = m_whiteSpace >> char '0' >> m_whiteSpace >> return []

reactant :: Parser (Ident, Nat)
reactant = one <|> more <?> "reactant"
  where
    one = (, 1) <$> m_identifier
    more = flip (,) <$> (fromIntegral <$> m_natural) <*> m_identifier

inits :: Parser [(Ident, Nat)]
inits = m_reserved "init" >> many initVal <?> "initial values"

initVal :: Parser (Ident, Nat)
initVal = (,) <$> m_identifier
              <*> (m_reservedOp "=" >> fromIntegral <$> m_natural)

modelParser :: Parser PModel
modelParser = do
  m_whiteSpace
  PModel <$> parameterList <*> speciesList <*> reactsList <*> inits

parseModel :: FilePath -> String -> Either ParseError Model
parseModel f s = B.second transformModel $ parse modelParser f s
