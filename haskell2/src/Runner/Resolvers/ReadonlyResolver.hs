module Runner.Resolvers.ReadonlyResolver where

import Model.Value
import Runner.Context
import Runner.PropertiesObject
import Runner.Resolvers.Resolver
import Runner.Resolvers.Eval

readonlyResolver :: PropertiesObject obj => Show obj => Resolver obj
readonlyResolver = Resolver {
    beforeHas   = \context       -> GNotResolved,
    afterHas    = \context hasIt -> GNotResolved,
    beforeGet   = \context       -> GNotResolved,
    afterGet    = \context value -> GNotResolved,
    beforeSet   = \context value -> 
        if (isReadonly context) then
            BSCancel
        else
            BSNotResolved,
    afterSet    = \context value -> ASNotResolved,
    beforeClear = \context       -> BSNotResolved,
    afterClear  = \context       -> ASNotResolved
}

isReadonly :: PropertiesObject obj => Show obj => Context obj -> Bool
isReadonly context = case (getReadonlyExpr context) of
            Just expr ->
                case (evalExpr context expr) of
                    Bool True -> True
                    _ -> False
            _ -> False