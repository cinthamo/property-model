module Runner.Resolvers.ValidResolver where

import Model.Value
import Runner.Context
import Runner.PropertiesObject
import Runner.Resolvers.Resolver
import Runner.Resolvers.Eval

validResolver :: PropertiesObject obj => Show obj => Resolver obj
validResolver = Resolver {
    beforeHas   = \context       -> GNotResolved,
    afterHas    = \context hasIt -> GNotResolved,
    beforeGet   = \context       -> GNotResolved,
    afterGet    = \context value -> GNotResolved,
    beforeSet   = \context value -> 
        if (isValid context value) then
            BSNotResolved
        else
            BSCancel,
    afterSet    = \context value -> ASNotResolved,
    beforeClear = \context       -> BSNotResolved,
    afterClear  = \context       -> ASNotResolved
}

isValid :: PropertiesObject obj => Show obj => Context obj -> Value obj -> Bool
isValid context value = case (getValidExpr context) of
            Just expr ->
                case (evalExpr (getContextWithValue context value) expr) of
                    Bool True -> True
                    _ -> False
            _ -> False
