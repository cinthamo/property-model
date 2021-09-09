module Model.Resolvers.EmptyResolver where

import Model.PropertiesObject
import Model.Resolvers.Resolver

emptyResolver :: PropertiesObject obj => Resolver obj
emptyResolver = Resolver {
    getAll      = \context names -> names,
    beforeHas   = \context       -> GNotResolved,
    afterHas    = \context hasIt -> GNotResolved,
    beforeGet   = \context       -> GNotResolved,
    afterGet    = \context value -> GNotResolved,
    beforeSet   = \context value -> BSNotResolved,
    afterSet    = \context value -> ASNotResolved,
    beforeClear = \context       -> BSNotResolved,
    afterClear  = \context       -> ASNotResolved
}