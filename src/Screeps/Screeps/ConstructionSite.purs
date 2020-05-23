-- | Corresponds to the Screeps API [ConstructionSite](http://support.screeps.com/hc/en-us/articles/203016342-ConstructionSite)
module Screeps.ConstructionSite where

import Prelude
import Effect (Effect)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)
import Screeps.Id (class HasId, decodeJsonWithId, encodeJsonWithId, id, eqById, validate)
import Screeps.Progress (class Progress)
import Screeps.Types (class Owned)
import Screeps.FFI (runThisEffectFn0, instanceOf)
import Screeps.ReturnCode (ReturnCode)
import Screeps.Structure (class Structural, structureType)
import Screeps.RoomObject (class RoomObject, pos, AnyRoomObject)

foreign import data ConstructionSite :: Type

instance constructionSiteIsRoomObject :: RoomObject ConstructionSite

instance constructionSiteIsStructural :: Structural ConstructionSite

instance constructionSiteProgress :: Progress ConstructionSite

instance constructionSiteIsOwned :: Owned ConstructionSite

instance constructionSiteHasId :: HasId ConstructionSite where
  validate = instanceOf "ConstructionSite"

instance encodeConstructionSite :: EncodeJson ConstructionSite where
  encodeJson = encodeJsonWithId

instance decodeConstructionSite :: DecodeJson ConstructionSite where
  decodeJson = decodeJsonWithId

instance eqConstructionSite :: Eq ConstructionSite where
  eq = eqById

instance showConstructionSite :: Show ConstructionSite where
  show c =
    "construction of " <> show (structureType c)
      <> "@"
      <> show (pos c)
      <> " ["
      <> show (id c)
      <> "]"

remove :: ConstructionSite -> Effect ReturnCode
remove = runThisEffectFn0 "remove"

toConstructionSite ::
  AnyRoomObject ->
  Maybe ConstructionSite
toConstructionSite ro =
  if validate cons then
    Just cons
  else
    Nothing
  where
  cons :: ConstructionSite
  cons = unsafeCoerce ro
