module Halogen.Fnords.MapboxPlaces
  ( component
  , ComponentEff
  , Query(..)
  , Message(..)
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Traversable (for)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Components.Html5Autosuggest as Autosuggest
import Control.Monad.Eff.Random as RAND

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)

import Network.HTTP.Affjax as AX

data Query a = HandleInput Autosuggest.Message a

data Message = Place String

data Slot = AutosuggestSlot
derive instance eqAutosuggestSlot :: Eq Slot
derive instance ordAutosuggestSlot :: Ord Slot

type State = { key :: String, suggestions :: Array String, search :: String }

type ComponentEff eff = (console :: CONSOLE, ajax :: AX.AJAX, random :: RAND.RANDOM | eff )

component :: forall m. H.Component HH.HTML Query String Message (Aff (ComponentEff m))
component =
  H.parentComponent
    { initialState: initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: String -> State
  initialState k = { key: k, suggestions: [], search: "" }

  render :: State -> H.ParentHTML Query Autosuggest.Query Slot (Aff (ComponentEff m))
  render state =
    HH.div_
      [ HH.slot AutosuggestSlot  Autosuggest.component state.suggestions (HE.input HandleInput)
      ]

  eval :: Query ~> H.ParentDSL State Query Autosuggest.Query Slot Message (Aff (ComponentEff m))
  eval (HandleInput (Autosuggest.Search s) next) = do
      state <- H.get
      do
        (res :: AX.AffjaxResponse Json) <- H.liftAff $ AX.get ("https://api.mapbox.com/geocoding/v5/mapbox.places/" <> s <> ".json?access_token=" <> state.key)
        case getFeatures res.response of
          Left _ -> H.put {key: state.key, suggestions: [], search: s}
          Right as -> H.put {key: state.key, suggestions: as, search: s}
        H.raise $ Place s
        pure next

getFeatures :: Json -> Either String (Array String)
getFeatures json = do
  obj <- decodeJson json
  features <- obj .? "features"
  for features \itemJson -> do
    itemObj <- decodeJson itemJson
    itemObj .? "place_name"
