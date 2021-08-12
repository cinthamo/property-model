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

data PropertiesWithResolver resolver = PR resolver (Map Name (Value (PropertiesWithResolver r)))
data PropertiesWithListener resolver = PR resolver (Map Name (Value (PropertiesWithResolver r)))


class Object where
    get 
    put 


getContext :: PropertiesObject obj => obj -> obj
getContext obj = set "instance" (Obj obj)
    (set "definition" (Data "") empty)

instance PropertyObject PropertiesWithResolver where
    get name obj@(PR resolver map) =//
        let context = getContext obj
        let (bResolved, bValue) = beforeGet context name resolver obj
        if (bResolved)
            return bValue
            else
                let value = Map.lookup name map                
                let (aResolved, aValue) = afterGet context name value resolver obj
                if (afterResolved)
                    return aValue
                    else
                        return aValue



instance PropertiesObject PropertiesWithListener where
    get name obj =

instance PropertiesObject PropertiesFull where
    get name obj =