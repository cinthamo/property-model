module Model.Resolvers.ReadonlyResolver where

import Model.PropertiesObject
import Model.Context
import Model.Value
import Model.Resolvers.Resolver
import Model.Resolvers.Eval

readonlyResolver :: PropertiesObject obj => Resolver obj
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

isReadonly :: PropertiesObject obj => Context obj -> Bool
isReadonly context = case (getReadonlyExpr context) of
            Just expr ->
                case (evalExpr context expr) of
                    Bool True -> True
                    _ -> False
            _ -> False