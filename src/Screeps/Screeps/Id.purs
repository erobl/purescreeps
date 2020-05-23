module Screeps.Id
  ( Id(..)
  , class HasId
  , id
  , validate
  , unsafeGetObjectById -- until Deposit is in the API
  , getObjectById
  , encodeJsonWithId
  , decodeJsonWithId
  , eqById
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Screeps.FFI (unsafeField)

class HasId a where
  -- Check that object is valid
  validate :: a -> Boolean

newtype Id a
  = Id String

-- | Get a unique id of an object.
id :: forall a. HasId a => a -> Id a
id = unsafeField "id"

-- | Get the object from an Id, if it passes validation.
getObjectById :: forall a. HasId a => Id a -> Either String a
getObjectById i = case unsafeGetObjectById i of
  Nothing -> Left ("Object with id " <> show i <> " no longer exists")
  Just o
    | validate o -> Right o
  Just _ -> Left ("Object with given id failed type validation")

unsafeGetObjectById :: forall a. Id a -> Maybe a
unsafeGetObjectById = unsafeGetObjectById_helper Nothing Just

-- | This is unsafe method, for restoring objects by id stored in memory.
-- | WARNING: This is somewhat unsafe method, since the object is never checked for its typeEffectect
foreign import unsafeGetObjectById_helper :: forall a r. r -> (a -> r) -> Id a -> r

-- | WARNING: This is somewhat unsafe method, since the object should be checked for its typeEffectect
--foreign import unsafeGetObjectByIdEffect :: forall a. Effect (Id a) -> (Maybe a)
derive instance genericId :: Generic (Id a) _

derive newtype instance eqId :: Eq (Id a)

instance showId :: Show (Id a) where
  show (Id i) = "Id #" <> i

-- | Encode and decode as JSON String.
instance decodeJsonId :: DecodeJson (Id a) where
  decodeJson json = Id <$> decodeJson json

instance encodeJsonId :: EncodeJson (Id a) where
  encodeJson (Id a) = encodeJson a

eqById ::
  forall a.
  HasId a =>
  a ->
  a ->
  Boolean
eqById = (==) `on` id

-- Type For making class instances of objects with `HasId` easily:
encodeJsonWithId :: forall a. HasId a => a -> Json
encodeJsonWithId a = encodeJson (id a)

decodeJsonWithId :: forall a. HasId a => Json -> Either String a
decodeJsonWithId = decodeJson >=> getObjectById
