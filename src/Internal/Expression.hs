module Internal.Expression (module Internal.Expression) where

{- CFG for mathematical expressions

    expr     --> expr + term        |  expr - term   |  term
    term     --> term * factor      |  term / factor |  factor
    factor   --> - factor           |  operand
    operand  --> exponent ^ operand |  exponent
    exponent --> SQRT exponent      |  final
    final    --> LITERAL            |  PI            |  E      |  ( expr )
-}

data Expr
  = ExprPlus Expr Term
  | ExprMinus Expr Term
  | ExprTerm Term
  deriving (Show)

data Term
  = TermTimes Term Factor
  | TermDivide Term Factor
  | TermFactor Factor
  deriving (Show)

data Factor
  = FactorNegate Factor
  | FactorOperand Operand
  deriving (Show)

data Operand
  = OperandPow Exponent Operand
  | OperandExponent Exponent
  deriving (Show)

data Exponent
  = ExponentSqrt Exponent
  | ExponentFinal Final
  deriving (Show)

data Final
  = Literal Double
  | Pi
  | E
  | ExprInParentheses Expr
  deriving (Show)

-- Evaluates the AST

evalExpr :: Expr -> Double
evalExpr (ExprPlus e t) = evalExpr e + evalTerm t
evalExpr (ExprMinus e t) = evalExpr e - evalTerm t
evalExpr (ExprTerm t) = evalTerm t

evalTerm :: Term -> Double
evalTerm (TermTimes t f) = evalTerm t * evalFactor f
evalTerm (TermDivide t f) = evalTerm t / evalFactor f
evalTerm (TermFactor f) = evalFactor f 

evalFactor :: Factor -> Double
evalFactor (FactorNegate f) = negate $ evalFactor f
evalFactor (FactorOperand o) = evalOperand o

evalOperand :: Operand -> Double
evalOperand (OperandPow e o) = evalExponent e ** evalOperand o
evalOperand (OperandExponent e) = evalExponent e

evalExponent :: Exponent -> Double
evalExponent (ExponentSqrt e) = sqrt $ evalExponent e
evalExponent (ExponentFinal f) = evalFinal f

evalFinal :: Final -> Double
evalFinal (Literal d) = d
evalFinal Pi = pi
evalFinal E = exp 1
evalFinal (ExprInParentheses e) = evalExpr e