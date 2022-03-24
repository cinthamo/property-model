module Runner.Resolvers.Resolver where

import Data.Map
import Model.Definition
import Model.Value

data Context obj = Context
  { refTable :: RefTable obj,
    objects :: Map Name obj,
    definition :: Maybe Definition,
    value :: Maybe (Value obj),
    name :: Name
  }
  deriving Show

type Function obj = [Value obj] -> Value obj

data RefValue obj
  = RefObj obj
  | RefRes (Resolver obj)
  | RefFunc (Function obj) [ValueType]

instance Show (RefValue obj) where
  show (RefObj _) = "RefObj"
  show (RefRes _) = "RefRes"
  show (RefFunc _ _) = "RefFunc"

type RefTable obj = Map Name (RefValue obj)

data ResolveGet value = GNotResolved | GResolved value

data ResolveBeforeSet value = BSNotResolved | BSCancel | BSValue value

data ResolveAfterSet = ASNotResolved | ASResolved

data Void

data Resolver obj = Resolver
  { beforeHas :: Context obj -> ResolveGet Bool,
    afterHas :: Context obj -> Bool -> ResolveGet Bool,
    beforeGet :: Context obj -> ResolveGet (Value obj),
    afterGet :: Context obj -> Maybe (Value obj) -> ResolveGet (Value obj),
    beforeSet :: Context obj -> Value obj -> ResolveBeforeSet (Value obj),
    afterSet :: Context obj -> Value obj -> ResolveAfterSet,
    beforeClear :: Context obj -> ResolveBeforeSet Void,
    afterClear :: Context obj -> ResolveAfterSet
  }
