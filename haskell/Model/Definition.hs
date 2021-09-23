module Model.Definition where

import Model.Const
import Model.Value
import Data.Map

data ObjectDefinition =
    ObjectDefinition {
      properties :: [Definition],
      related :: [Name]
    }

data Definition =
    Definition {
        name :: Name,
        _default :: Expr
    } |
    External {
        name :: Name
    }

newtype ExprValue = V (Value (Map Name (Value ExprValue)))

data Expr = Value ExprValue |
            Ref Name Name |
            Case [If] Expr |
            Func Name [Expr] -- pure functions, f :: [Value] -> Value

data If = If Expr Expr

-- helpers
num :: Int -> Expr
num n = Value $ V $ Number n

str :: String -> Expr
str s = Value $ V $ String s

false :: Expr
false = Value $ V $ Bool False

true :: Expr
true = Value $ V $ Bool True