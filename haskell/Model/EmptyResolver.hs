module Model.EmptyResolver where

import Model.PropertiesObject
import Model.Resolver

emptyResolver :: PropertiesObject obj => Resolver obj
emptyResolver = Resolver {
    beforeHas   = \context       -> GNotResolved,
    afterHas    = \context hasIt -> GNotResolved,
    beforeGet   = \context       -> GNotResolved,
    afterGet    = \context value -> GNotResolved,
    beforeSet   = \context value -> BSNotResolved,
    afterSet    = \context value -> ASNotResolved,
    beforeClear = \context       -> BSNotResolved,
    afterClear  = \context       -> ASNotResolved
}