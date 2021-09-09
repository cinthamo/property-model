module Model.Resolvers.ChainResolver where

import Model.PropertiesObject
import Model.Const
import Model.Context
import Model.Resolvers.Resolver
import Data.List

chainResolver :: PropertiesObject obj => [Resolver obj] -> Resolver obj
chainResolver resolvers = Resolver {
    getAll      = \context names ->
        chainResolveAll resolvers context names,

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

chainResolveAll :: PropertiesObject obj => [Resolver obj] -> Context obj -> [Name] -> [Name]
chainResolveAll resolvers context list = nub (assignedList ++ definedList)
    where
        assignedList = filter (\n -> head n /= '@') list
        definedList = getDefinitionNames context