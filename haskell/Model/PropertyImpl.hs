module Model.PropertyImpl where

import Data.Map (Map)
import qualified Data.Map as Map
import Model.Property
import Model.Resolver

--- Property Bag ---

data PropertiesBag = PB (Map Name (Value PropertiesBag))

instance PropertiesObject PropertiesBag where
    get name (PB map) = Map.lookup name map
    set name value (PB map) = PB (Map.insert name value map)
    has name (PB map) = Map.member name map
    clear name (PB map) = PB (Map.delete name map)
    empty = PB Map.empty

--- Property With Resolvers ---

data PropertiesWithResolver r = PR r (Map Name (Value (PropertiesWithResolver r)))

getContext :: PropertiesObject obj => obj -> obj
getContext o = set "instance" (Obj o)
    (set "definition" (Data "") empty)
{-
instance PropertyObject PropertiesWithResolver where
    get name (PR r map) =
        let context = getContext this
        let (beforeResolved, beforeValue) = beforeGet context name r this
        if beforeResolved beforeValue else
            let value = Map.lookup name map
            let (afterResolved, afterValue) = afterGet context name value r this
            if (afterResolved) afterValue else value
-}