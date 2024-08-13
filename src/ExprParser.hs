module ExprParser (eval) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Internal.Expression
import Internal.Parser

-- Parses and evaluates a string as an expression

eval :: String -> Maybe Double
eval i = do
  (rest, e) <- run exprP i
  guard $ null rest
  return $ evalExpr e

-- Expression operator parsers

exprPlus :: Parser (Expr -> Term -> Expr)
exprPlus = spacesP >> charP '+' >> pure ExprPlus

exprMinusP :: Parser (Expr -> Term -> Expr)
exprMinusP = spacesP >> charP '-' >> pure ExprMinus

termTimesP :: Parser (Term -> Factor -> Term)
termTimesP = spacesP >> charP '*' >> pure TermTimes

termDivideP :: Parser (Term -> Factor -> Term)
termDivideP = spacesP >> charP '/' >> pure TermDivide

operandPowP :: Parser (Exponent -> Operand -> Operand)
operandPowP = spacesP >> charP '^' >> pure OperandPow

-- Expression type constructor parsers

exprP :: Parser Expr
exprP = chainLeft termP ExprTerm exprPlus exprMinusP

termP :: Parser Term
termP = chainLeft factorP TermFactor termTimesP termDivideP

factorP :: Parser Factor
factorP = (spacesP >> charP '-' >> FactorNegate <$> factorP) <|> (FactorOperand <$> operandP)

operandP :: Parser Operand
operandP = chainRight exponentP operandP operandPowP OperandExponent

exponentP :: Parser Exponent
exponentP =
  (spacesP >> stringP "sqrt" >> ExponentSqrt <$> exponentP)
    <|> (ExponentFinal <$> finalP)

finalP :: Parser Final
finalP =
  (spacesP >> Literal <$> doubleP <* spacesP)
    <|> (spacesP >> charP 'e' >> (E <$ spacesP))
    <|> (spacesP >> stringP "pi" >> (Pi <$ spacesP))
    <|> (spacesP >> charP '(' >> ExprInParentheses <$> exprP <* spacesP <* charP ')' <* spacesP)