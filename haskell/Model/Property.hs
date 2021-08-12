module Property where

import Data.Map (Map)
import qualified Data.Map as Map

type Name = String
data Value obj = Data String |
                 Ref Name |
                 Obj obj

class PropertyObject obj where
    get :: Name -> obj -> Maybe (Value obj)
    set :: Name -> Value obj -> obj -> obj
    has :: Name -> obj -> Bool
    delete :: Name -> obj -> obj

data PropertyBag = PB (Map Name (Value PropertyBag))

instance PropertyObject PropertyBag where
    get name (PB map) = Map.lookup name map
    set name value (PB map) = PB (Map.insert name value map)
    has name (PB map) = Map.member name map
    delete name (PB map) = PB (Map.delete name map)