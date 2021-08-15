module Model.AspectBehaviour where

import Model.PropertiesObject
import Model.Resolver
import Model.EmptyResolver
import Model.Behaviour
import Model.Context
import Model.Const

emptyWAsp :: PropertiesObject obj => obj
emptyWAsp = empty (BResolver aspectResolver)

aspectResolver :: PropertiesObject obj => Resolver obj
aspectResolver = Resolver {
    beforeHas   = \context -> beforeHas   (chainResolver (getResolverList (context))) context,
    afterHas    = \context -> afterHas    (chainResolver (getResolverList (context))) context,
    beforeGet   = \context -> beforeGet   (chainResolver (getResolverList (context))) context,
    afterGet    = \context -> afterGet    (chainResolver (getResolverList (context))) context,
    beforeSet   = \context -> beforeSet   (chainResolver (getResolverList (context))) context,
    afterSet    = \context -> afterSet    (chainResolver (getResolverList (context))) context,
    beforeClear = \context -> beforeClear (chainResolver (getResolverList (context))) context,
    afterClear  = \context -> afterClear  (chainResolver (getResolverList (context))) context
}

getAspects :: PropertiesObject obj => obj -> [obj]
getAspects context =
    let def = getContextObj context cDEFINITION
    in case (get emptyObj def cMETA_ASPECTS) of
        Just (List l) -> map (\a -> getObject context a) l
        _ -> []

getResolverList :: PropertiesObject obj => obj -> [Resolver obj]
getResolverList context = map (\aspect ->
    case (get emptyObj aspect cASPECT_RESOLVER) of
        Just v -> getResolver context v
        _ -> emptyResolver) (getAspects context)

chainResolver :: PropertiesObject obj => [Resolver obj] -> Resolver obj
chainResolver resolvers = Resolver {
    beforeHas   = \context       ->
        chainResolveOne (\r -> (beforeHas r) context) resolvers isGResolved GNotResolved,

    afterHas    = \context hasIt ->
        chainResolveOne (\r -> (afterHas r) context hasIt) resolvers isGResolved GNotResolved,

    beforeGet   = \context       ->
        chainResolveOne (\r -> (beforeGet r) context) resolvers isGResolved GNotResolved,

    afterGet    = \context value ->
        chainResolveOne (\r -> (afterGet r) context value) resolvers isGResolved GNotResolved,

    beforeSet   = \context value ->
        chainResolveOne (\r -> (beforeSet r) context value) resolvers isBSResolved BSNotResolved,

    afterSet    = \context value ->
        chainResolveOne (\r -> (afterSet r) context value) resolvers isASResolved ASNotResolved,

    beforeClear = \context       ->
        chainResolveOne (\r -> (beforeClear r) context) resolvers isBSResolved BSNotResolved,

    afterClear  = \context       ->
        chainResolveOne (\r -> (afterClear r) context) resolvers isASResolved ASNotResolved
}

isGResolved :: ResolveGet v -> Bool
isGResolved GNotResolved = False
isGResolved _ = True

isBSResolved :: ResolveBeforeSet v -> Bool
isBSResolved BSNotResolved = False
isBSResolved _ = True

isASResolved :: ResolveAfterSet -> Bool
isASResolved ASResolved = False
isASResolved _ = True

chainResolveOne :: PropertiesObject obj => (Resolver obj -> result) -> [Resolver obj] -> (result -> Bool) -> result -> result
chainResolveOne f resolvers isResolved notResolved =
    let m = map f resolvers
        l = filter isResolved m
    in case (l) of
        [] -> notResolved
        x:_ -> x
