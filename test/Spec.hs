{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (chr, isAscii, isAsciiLower, isAsciiUpper, ord, toUpper)
import ExprParser (eval)
import Internal.Parser (charP, run)
import Numeric (showFFloat)
import Test.Hspec (context, describe, hspec, it, shouldBe)
import Test.QuickCheck (Arbitrary (arbitrary), Testable (property), forAll, suchThat, vectorOf)

showFloat :: Double -> String
showFloat d = showFFloat Nothing d ""

main :: IO ()
main = hspec $ do
  describe "Functor Laws" $ do
    context "Identity" $ do
      it "fmap id = id" $ property $ \c ->
        run (fmap id (charP c)) [c] == run (id (charP c)) [c]

    context "Composition" $ do
      it "fmap (f . g)  ==  fmap f . fmap g" $ property $ \c ->
        let f = ord
            g = toUpper
            p = charP c
         in run (fmap (f . g) p) [c] == run ((fmap f . fmap g) p) [c]

  describe "Applicative Laws" $ do
    context "Identity" $ do
      it "pure id <*> v = v" $ property $ \c ->
        run (pure id <*> (charP c)) [c] == run (charP c) [c]

    context "Homomorphism" $ do
      it "pure f <*> pure x = pure (f x)" $ property $ \x ->
        run (pure ord <*> pure x) "" == run (pure (ord x)) ""

    context "Interchange" $ do
      it "u <*> pure y = pure ($ y) <*> u" $ property $ \y ->
        let u = pure isAsciiLower
         in run (u <*> pure y) [y] == run (pure ($ y) <*> u) [y]

    context "Composition" $ do
      it "pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ property $ \c ->
        let u = pure chr
            v = pure ord
            w = charP c
         in run (pure (.) <*> u <*> v <*> w) [c] == run (u <*> (v <*> w)) [c]

  describe "Alternative Laws" $ do
    context "Right distributivity (of <*>)" $ do
      it "(f <|> g) <*> a = (f <*> a) <|> (g <*> a)" $ property $ \x ->
        let f = pure isAsciiLower
            g = pure isAsciiUpper
            a = charP x
         in run ((f <|> g) <*> a) [x] == run ((f <*> a) <|> (g <*> a)) [x]

    context "Right absorption (for <*>)" $ do
      it "empty <*> a = empty" $ property $ \x ->
        run (empty <*> charP x) [x] == (run empty [x] :: Maybe (String, Char))

    context "Left distributivity (of fmap)" $ do
      it "f <$> (a <|> b) = (f <$> a) <|> (f <$> b)" $ property $ \x y ->
        let f = isAsciiLower
            a = charP x
            b = charP y
         in run (f <$> (a <|> b)) [x, y] == run ((f <$> a) <|> (f <$> b)) [x, y]

    context "Left absorption (for fmap)" $ do
      it "f <$> empty = empty" $
        run (isAscii <$> empty) "" `shouldBe` run empty ""

  describe "Monad Laws" $ do
    context "Left identity" $ do
      it "return a >>= h == h a" $ property $ \a ->
        run (return a >>= (charP)) [a] == run (charP a) [a]

    context "Right identity" $ do
      it "m >>= return == m" $ property $ \a ->
        run (charP a >>= return) [a] == run (charP a) [a]

    context "Associativity" $ do
      it "(m >>= g) >>= h == m >>= (\\x -> g x >>= h)" $ property $ \a ->
        let g = charP
            h = charP
            m = charP a
         in run ((m >>= g) >>= h) [a] == run (m >>= (\x -> g x >>= h)) [a]

  describe "Expressions" $ do
    context "Addition" $ do
      it "a + b + c" $ property $ \a b c ->
        eval (showFloat a ++ "+" ++ showFloat b ++ " + " ++ showFloat c) == Just (a + b + c)

    context "Subtraction" $ do
      it "a - b - c" $ property $ \a b c ->
        eval (showFloat a ++ "-" ++ showFloat b ++ " -" ++ showFloat c) == Just (a - b - c)

    context "Multiplication" $ do
      it "a * b * c" $ property $ \a b c ->
        eval (showFloat a ++ "*" ++ showFloat b ++ " *" ++ showFloat c) == Just (a * b * c)

    -- returns division by 0, use isNaN on the result to handle this case,
    -- see the updateState in Main.hs for reference.
    context "Division" $ do
      it "a / b / c" $
        property $
          forAll (vectorOf 2 (arbitrary `suchThat` (> 0))) $
            \[a, b] c ->
              eval (showFloat a ++ "/" ++ showFloat b ++ "/" ++ showFloat c) == Just (a / b / c)

    context "Exponentiation" $ do
      it "a ^ b" $
        property $
          forAll (vectorOf 2 (arbitrary `suchThat` (> 0))) $
            \[a, b] ->
              eval (showFloat a ++ "^ " ++ showFloat b) == Just (a ** b)

    context "Exponentiation with unary minus" $ do
      it "a ^ (-b) ^ (-c)" $
        property $
          forAll (vectorOf 3 (arbitrary `suchThat` (> 0))) $
            \[a :: Int, b :: Int, c :: Int] ->
              eval (show a ++ "^ (" ++ show (-b) ++ ") ^ (" ++ show (-c) ++ ")")
                == Just (fromIntegral a ** fromIntegral (-b) ** fromIntegral (-c))

    context "Unary minus" $ do
      it "-a" $ property $ \a ->
        eval ("-" ++ showFloat a) == Just (-a)

    context "Unary minus" $ do
      it "----a" $ property $ \a ->
        eval ("----" ++ showFloat a) `shouldBe` Just a

    context "Sqrt" $ do
      it "sqrt(a)" $ property $ forAll (arbitrary `suchThat` (> 0)) $ \a ->
        eval ("sqrt(" ++ showFloat a ++ ")") == Just (sqrt a)

    context "Arithmetic expression" $ do
      it "b + c - d * e / a" $ property $ forAll (arbitrary `suchThat` (> 0)) $ \a b c d e ->
        eval (showFloat b ++ " + " ++ showFloat c ++ " - " ++ showFloat d ++ " * " ++ showFloat e ++ " /" ++ showFloat a)
          == Just (b + c - d * e / a)

    context "unbalanced parentheses" $ do
      it "(1 + 2" $ do
        eval "(1 + 2" `shouldBe` Nothing

    context "unbalanced parentheses" $ do
      it "1 + 2)" $ do
        eval "1 + 2)" `shouldBe` Nothing

    context "comma instead of dot" $ do
      it "a,b" $ property $ \n ->
        let (a, b) = span (/= '.') (showFloat n)
         in eval (a ++ "," ++ tail b) == Just n