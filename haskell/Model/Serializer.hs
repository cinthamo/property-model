module Model.Serializer where

import Model.PropertiesObject as PO
import Model.Behaviour
import Model.Value as V
import Model.Resolvers.AspectResolver
import Data.Aeson as JSON
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.HashMap.Strict as HM
import Data.Map as M

fromString :: PropertiesObject obj => String -> obj
fromString s =
    case (JSON.decode (BLU.fromString s) :: Maybe JSON.Value) of
        Just (JSON.Object m) -> fromMap m
        _ -> PO.empty BEmpty

fromJson :: PropertiesObject obj => JSON.Value -> V.Value obj
fromJson (JSON.Object m) = V.Object (fromMap m)
fromJson (Array a) = List (Prelude.map (\v -> fromJson v) (V.toList a))
fromJson (JSON.String t) = fromText t
fromJson (JSON.Number n) = V.Number n
fromJson (JSON.Bool b) = V.Bool b
fromJson Null = V.String ""

fromText :: T.Text -> V.Value obj
fromText t =
    let s = T.unpack t
    in if head s == '^' then
        let s1 = tail s
        in if head s1 == '^' then
            V.String s1
        else
            V.Reference s1
    else
        V.String s

fromMap :: PropertiesObject obj => HashMap T.Text JSON.Value -> obj
fromMap m = HM.foldrWithKey f emptyWAsp m
    where
        f name value o = set M.empty o (T.unpack name) (fromJson value)