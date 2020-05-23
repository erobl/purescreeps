-- | Corresponds to the Screeps API [StructureExtension](http://support.screeps.com/hc/en-us/articles/207711949-StructureExtension)
module Screeps.Extension where

import Screeps.Structure
import Data.Eq (class Eq)
import Data.Ord ((>))
import Data.Show (class Show)
import Screeps.Types (class Owned)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Map (lookup)
import Data.Maybe (Maybe, fromMaybe)
import Screeps.Constants (extension_energy_capacity)
import Screeps.Destructible (class Destructible)
import Screeps.FFI (instanceOf)
import Screeps.Id (class HasId, decodeJsonWithId, encodeJsonWithId, eqById)
import Screeps.Refillable (class Refillable)
import Screeps.RoomObject (class RoomObject)

foreign import data Extension :: Type

instance objectExtension :: RoomObject Extension

instance ownedExtension :: Owned Extension

instance extensionHasId :: HasId Extension where
  validate = instanceOf "StructureExtension"

instance encodeExtension :: EncodeJson Extension where
  encodeJson = encodeJsonWithId

instance decodeExtension :: DecodeJson Extension where
  decodeJson = decodeJsonWithId

instance structuralExtension :: Structural Extension

instance refillableExtension :: Refillable Extension

instance eqExtension :: Eq Extension where
  eq = eqById

instance showExtension :: Show Extension where
  show = showStructure

instance structureExtension :: Structure Extension where
  _structureType _ = structure_extension

instance destructibleExtension :: Destructible Extension

toExtension :: AnyStructure -> Maybe Extension
toExtension = fromAnyStructure

-- | Extension capacity on a given room control level.
extensionCapacity :: Int -> Int
extensionCapacity level
  | level > 8 = extensionCapacity 8

extensionCapacity level =
  0
    `fromMaybe`
      lookup level extension_energy_capacity
