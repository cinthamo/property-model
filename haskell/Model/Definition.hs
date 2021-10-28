module Model.Definition where

import Data.Map
import Model.Const
import Model.Value

data ObjectDefinition = ObjectDefinition
  { properties :: [Definition],
    related :: [Name]
  }

data Definition
  = Definition
      { name :: Name,
        _type :: Name,
        _default :: Expr,
        apply :: Expr,
        readonly :: Expr,
        valid :: Expr
      }
  | External
      { name :: Name
      }

newtype ExprValue = V (Value (Map Name (Value ExprValue)))

data Expr
  = Value ExprValue
  | Ref Name Name -- object.property
  | RefValue -- setting value
  | ObjRef Name -- object
  | Case [(Expr, Expr)] (Maybe Expr) -- conditions otherwise
  | Call Name [Expr] -- f :: [Value] -> Value,
  -- methodName object++parameters, External Object Method Call
  -- procedureName parameters, GX Procedure Call
  -- functionName parameters, language supported functions

-- smart constructors
num :: Int -> Expr
num n = Value $ V $ Number n

str :: String -> Expr
str s = Value $ V $ String s

false :: Expr
false = Value $ V $ Bool False

true :: Expr
true = Value $ V $ Bool True

emptyValue :: Expr
emptyValue = Value $ V $ String ""