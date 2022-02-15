module Runner.Resolvers.AspectResolver where

import Debug.Trace
import Model.Definition
import Model.Value
import Runner.PropertiesObject
import Runner.Context
import Runner.Const
import Runner.Resolvers.Resolver
import Runner.Resolvers.ChainResolver
import Runner.Resolvers.DefaultResolver
import Runner.Resolvers.ApplyResolver
import Runner.Resolvers.ReadonlyResolver
import Runner.Resolvers.ValidResolver

emptyWAsp :: PropertiesObject obj => DefinitionList -> obj
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
getResolverList _ = [ defaultResolver, applyResolver, readonlyResolver, validResolver ]