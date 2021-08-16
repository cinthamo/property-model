module Model.DefaultResolver where

import Model.PropertiesObject
import Model.Resolver
import Model.Const
import Model.Context

defaultResolver :: PropertiesObject obj => Resolver obj
defaultResolver = Resolver {
    beforeHas   = \context       -> GNotResolved,
    afterHas    = \context hasIt ->
        if hasIt then
            GNotResolved
        else
            let definition = getContextObj context cDEFINITION
                _default = get (refTable context) definition cDEFAULT
            in case _default of
                Just v -> GResolved True
                _ -> GNotResolved,

    beforeGet   = \context       -> GNotResolved,
    afterGet    = \context value ->
        case value of
            Just v -> GNotResolved
            _ ->
                let definition = getContextObj context cDEFINITION
                    _default = get (refTable context) definition cDEFAULT
                in case _default of
                    Just v -> GResolved v
                    _ -> GNotResolved,

    beforeSet   = \context value -> BSNotResolved,
    afterSet    = \context value -> ASNotResolved,
    beforeClear = \context       -> BSNotResolved,
    afterClear  = \context       -> ASNotResolved
}