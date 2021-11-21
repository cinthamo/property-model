module Func.Static where

import Model.PropertiesObject as PO
import Model.Behaviour
import Model.Resolvers.Resolver

static :: PropertiesObject obj => RefTable obj -> obj -> obj
static refTable objIn = foldr f emptyObj (PO.all refTable objIn)
    where
        f n objOut = case (get refTable objIn n) of
            Just v -> set refTable objOut n v
            Nothing -> objOut