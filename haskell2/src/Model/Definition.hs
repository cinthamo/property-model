module Model.Definition where

import Data.Map
import Model.Value

data DefinitionList = DefinitionList
  { lname :: Name,
    externalType :: Maybe Name, -- external type of this list
    properties :: [Definition]
  }
  deriving (Eq, Ord, Show)

data Definition
  = Definition
      { name :: Name,
        _type :: ValueType,
        _default :: Expr,
        apply :: Expr,
        readonly :: Expr,
        valid :: Expr
      }
  deriving (Eq, Ord, Show)

newtype ExprValue = V (Value (Map Name (Value ExprValue)))
  deriving (Eq, Ord, Show)

data Expr
  = Value ExprValue
  | ValueRef -- setting value
  | NameRef Name -- object
  | PropRef Expr Name -- object.property
  | Case [(Expr, Expr)] (Maybe Expr) -- conditions otherwise
  | Call Name [Expr] -- f :: [Value] -> Value,
  -- methodName object++parameters, External Object Method Call
  -- procedureName parameters, GX Procedure Call
  -- functionName parameters, language supported functions
  deriving (Eq, Ord, Show)

-- smart constructors
num :: Int -> Expr
num n = Value $ V $ Number n

str :: String -> Expr
str s = Value $ V $ String s

false :: Expr
false = Value $ V $ Bool False

true :: Expr
true = Value $ V $ Bool True

null :: Expr
null = Value $ V Null

emptyValue :: Expr
emptyValue = Value $ V $ String ""