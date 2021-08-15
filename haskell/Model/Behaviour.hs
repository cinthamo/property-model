module Model.Behaviour where

import Model.PropertiesObject
import Model.Resolver

emptyResolver :: PropertiesObject obj => Resolver obj
emptyResolver = Resolver
    (\x y -> GNotResolved)
    (\x y z -> GNotResolved)
    (\x y -> GNotResolved)
    (\x y z -> GNotResolved)
    (\x y z -> BSNotResolved)
    (\x y z -> ASNotResolved)
    (\x y -> BSNotResolved)
    (\x y -> ASNotResolved)

resolver :: PropertiesObject obj => Behaviour obj -> Resolver obj
resolver BEmpty = emptyResolver
resolver (BResolver r) = r

emptyB :: PropertiesObject obj => obj
emptyB = empty BEmpty