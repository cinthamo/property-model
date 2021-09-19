module Model.Resolvers.AspectResolver where

import Model.PropertiesObject
import Model.Context
import Model.Const
import Model.Definition
import Model.Value
import Model.Resolvers.Resolver
import Model.Resolvers.ChainResolver
import Model.Resolvers.DefaultResolver
import Debug.Trace

emptyWAsp :: PropertiesObject obj => ObjectDefinition -> obj
emptyWAsp = empty (BResolver aspectResolver)

aspectResolver :: PropertiesObject obj => Resolver obj
aspectResolver = Resolver {
    beforeHas   = \context -> beforeHas   (chainResolver (getResolverList context)) context,
    afterHas    = \context -> afterHas    (chainResolver (getResolverList context)) context,
    beforeGet   = \context -> beforeGet   (chainResolver (getResolverList context)) context,
    afterGet    = \context -> afterGet    (chainResolver (getResolverList context)) context,
    beforeSet   = \context -> beforeSet   (chainResolver (getResolverList context)) context,
    afterSet    = \context -> afterSet    (chainResolver (getResolverList context)) context,
    beforeClear = \context -> beforeClear (chainResolver (getResolverList context)) context,
    afterClear  = \context -> afterClear  (chainResolver (getResolverList context)) context
}

getResolverList :: PropertiesObject obj => Context obj -> [Resolver obj]
getResolverList _ = [ defaultResolver ]