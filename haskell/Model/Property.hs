module Model.Property where

type Name = String
data Value obj = Data String |
                 Ref Name |
                 Obj obj

class PropertiesObject obj where
    get :: Name -> obj -> Maybe (Value obj)
    set :: Name -> Value obj -> obj -> obj
    has :: Name -> obj -> Bool
    clear :: Name -> obj -> obj
    empty :: obj


data PropertiesBag x = PB (Map Name (Value PropertiesBag))
data PropertiesWithResolver resolver = PR resolver (Map Name (Value (PropertiesWithResolver resolver)))
data PropertiesWithListener listener = PL listener (Map Name (Value (PropertiesWithListener listener)))
data PropertiesFull resolver listener = PF resolver listener (Map Name (Value (PropertiesFull resolver listener)))

instance PropertyObject PropertiesWithResolver where
    get name obj@(PR resolver map) = --resolver (map)

instance PropertyObject PropertiesWithListener where
    get name obj@(PL listener map) = --listener (map)

instance PropertyObject PropertiesFull where
    get name obj@(PF resolver listener map) = --listener (resolver (map))