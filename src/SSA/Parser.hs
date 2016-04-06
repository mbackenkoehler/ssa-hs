{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RecordWildCards #-}
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
      ( parseModel, ModelParserError(..)
      ) where
------------------------------------------------------------------------------
import           Control.Monad
import qualified Data.Bifunctor as B
import           Data.List ( group )
import           Text.Parsec hiding ( State )
import           Text.Parsec.Expr
import           Text.Parsec.String
import           Text.Parsec.Token
import           Text.Parsec.Language
------------------------------------------------------------------------------
import           SSA.Model
import           SSA.Expr
------------------------------------------------------------------------------
data ModelParserError
  = ParsecError ParseError
  | NameError String

instance Show ModelParserError where
  show (ParsecError err) = show err
  show (NameError msg) = msg
------------------------------------------------------------------------------
TokenParser
  { parens     = m_parens
  , identifier = m_identifier
  , reservedOp = m_reservedOp
  , reserved   = m_reserved
  , semiSep1   = m_semiSep1
  , whiteSpace = m_whiteSpace
  , float      = m_float
  , integer    = m_integer
  , natural    = m_natural
  } = makeTokenParser $ emptyDef
  { commentLine     = "--"
  , identStart      = letter
  , identLetter     = alphaNum
  , opStart         = oneOf "+-*/@;:"
  , opLetter        = oneOf "+>-*/@;:"
  , reservedOpNames = ["+","-","*","/","@",";",":","->"]
  , reservedNames   = ["parameters","species","int","bool","boolean","const"
                      ,"init","var","reactions"]
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
parameterList = constKeyword >> many parameter <?> "parameters"
  where constKeyword = m_reserved "parameters" <|> m_reserved "const"

parameter :: Parser (Ident, Double)
parameter = (,) <$> m_identifier <*> (m_reservedOp "=" >> num)

speciesList :: Parser [Species]
speciesList = varKeyword >> many specie <?> "species"
  where varKeyword = m_reserved "species" <|> m_reserved "var"

specie :: Parser Species
specie = flip Species <$> m_identifier <*> (m_reservedOp ":" >> typ)

typ :: Parser SpeciesType
typ = (hcType >> pure HCopy) <|> (lcType >> pure LCopy)
  where
    hcType = m_reserved "int" <|> m_reserved "species"
    lcType = m_reserved "bool" <|> m_reserved "boolean"

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

parseModel :: FilePath -> String -> Either ModelParserError Model
parseModel f s = B.second transformModel . checks $ B.first ParsecError pmodel
  where
    pmodel = parse modelParser f s

    checks e@(Left _) = e
    checks (Right m@PModel{..}) = do
      let sNames = (\(Species _ n) -> n) <$> species
          idents = (fst <$> parameters) ++ sNames
          propensities = (\(PReaction _ _ e) -> e) <$> reacts
          reactants = fst <$> concatMap (\(PReaction i o _) -> i++o) reacts
      unless (unique sNames) (Left (NameError "Species names not unique"))
      forM_ propensities (unboundId idents)
      forM_ reactants (isElem sNames)
      return m

    unique = all (== 1) . fmap length . group

    unboundId names e =
        when (anyExpr (unbound names) e) $
          Left . NameError $ "Unbound identifier in rate expression"

    unbound names (Atomic (Var n)) = n `notElem` names
    unbound _ _ = False

    isElem xs e
      | e `elem` xs = Right ()
      | otherwise = Left . NameError $ "Unkown species: " ++ e
