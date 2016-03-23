{-# LANGUAGE LambdaCase #-}
------------------------------------------------------------------------------
import           Data.Either
import qualified Data.Text            as T
import qualified Data.Vector.Unboxed  as V
import           Test.Hspec
import qualified Test.QuickCheck      as Q
------------------------------------------------------------------------------
import qualified SSA.Parser as P
import qualified SSA.Model  as M
import qualified SSA.Expr   as E
------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
  testParser
  testExpr
  testParserExpr

testParser :: Spec
testParser = do
  describe "Parser" $ do
    it "empty model 1" $ do
      let model = "parameters species reactions init"
          Right m = P.parseModel "(none)" model
      emptyModel m
    it "empty model 2" $ do
      let model = "const var reactions init"
          Right m = P.parseModel "(none)" model
      emptyModel m
    it "parses high copy species" $ do
      let model = "const var x : species y : int reactions init"
          Right m = P.parseModel "(none)" model
      M.names m `shouldMatchList` fmap T.pack ["x","y"]
    it "parses low copy species" $ do
      let model = "const var x : bool y : boolean reactions init"
          Right m = P.parseModel "(none)" model
      M.names m `shouldMatchList` fmap T.pack ["x","y"]
  describe "Name check tests" $ do
    it "checks reactant names" $ do
      let model = "const var x : int reactions 0 -> y @ 1; init"
      isNameError $ P.parseModel "(none)" model
    it "checks unresolved constants" $ do
      let model = "const var reactions 0 -> 0 @ x; init"
      isNameError $ P.parseModel "(none)" model
  where
    emptyModel m = do
      M.names m `shouldBe` []
      M.reactions m `shouldSatisfy` null
      M.initial m `shouldSatisfy` V.null
    isNameError res = do
      res `shouldSatisfy` isLeft
      let Left err = res
      case err of
        P.NameError _ -> pure ()
        _ -> expectationFailure "expected a SSA.Parser.NameError"

testExpr :: Spec
testExpr =
  describe "Propensity functions" $ do
    it "evaluates an atomic expression" $ do
      let e = E.Atomic (E.Id 0)
      Q.property $ \x -> M.evalV e (V.singleton x) == fromIntegral x
    it "evaluates a simple expression" $ do
      let e = E.BinOp E.Add (E.Atomic (E.Id 0)) (E.Atomic (E.Id 1))
          s x = V.fromList [fst x, snd x]
      Q.property $ \x -> M.evalV e (s x) == fromIntegral (uncurry (+) x)

testParserExpr :: Spec
testParserExpr =
  describe "Parser / Expr integration" $ do
    it "parses and evaluates 0.5 * x" $ do
      let model = "const var x : int reactions 0 -> 0 @ 0.5 * x; init"
      Q.property $ \x -> r model x == 0.5 * fromIntegral x
    it "parses and evaluates 0.5 * (x + 1) - 1" $ do
      let model = "const k = 0.5 var x : int \
                  \reactions 0 -> 0 @ k * (x + 1) - 1; init"
      Q.property $ \x -> r model x == 0.5 * fromIntegral (x + 1) - 1
  where
    r model = let Right m = P.parseModel "(none)" model
               in (M.propensity . head . M.reactions) m . V.singleton
