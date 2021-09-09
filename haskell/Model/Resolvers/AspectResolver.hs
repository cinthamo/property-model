module Model.Resolvers.AspectResolver where

import Model.PropertiesObject
import Model.Context
import Model.Const
import Model.Value
import Model.Resolvers.Resolver
import Model.Resolvers.EmptyResolver
import Model.Resolvers.ChainResolver
import Debug.Trace

emptyWAsp :: PropertiesObject obj => obj
emptyWAsp = empty (BResolver aspectResolver)

aspectResolver :: PropertiesObject obj => Resolver obj
aspectResolver = Resolver {
    getAll      = \context -> getAll      (chainResolver (getResolverList context)) context,
    beforeHas   = \context -> beforeHas   (chainResolver (getResolverList context)) context,
    afterHas    = \context -> afterHas    (chainResolver (getResolverList context)) context,
    beforeGet   = \context -> beforeGet   (chainResolver (getResolverList context)) context,
    afterGet    = \context -> afterGet    (chainResolver (getResolverList context)) context,
    beforeSet   = \context -> beforeSet   (chainResolver (getResolverList context)) context,
    afterSet    = \context -> afterSet    (chainResolver (getResolverList context)) context,
    beforeClear = \context -> beforeClear (chainResolver (getResolverList context)) context,
    afterClear  = \context -> afterClear  (chainResolver (getResolverList context)) context
}

getAspects :: PropertiesObject obj => Context obj -> [obj]
getAspects context =
    case (getDefinitionValue context cMETA_ASPECTS) of
        Just (List l) -> map (\a -> getObject (refTable context) a) l
        _ -> []

getResolverList :: PropertiesObject obj => Context obj -> [Resolver obj]
getResolverList context =
    map
        (\aspect ->
            case (get (refTable context) aspect cASPECT_RESOLVER) of
                Just v -> getResolver (refTable context) v
                _ -> emptyResolver)
        (getAspects context)
