module Halogen.Fnords.Html5Autosuggest
  ( component
  , ComponentEff
  , Query(..)
  , Message(..)
  ) where

import Prelude

import Data.Char (fromCharCode)
import Data.Maybe (Maybe(..))
import Data.Array (replicate, fromFoldable)
import Data.Traversable (sequence)
import Data.String (trim, fromCharArray)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random as RAND

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a = Initialize a | HandleInput String a | UpdateSuggestions (Array String) a

type State = { uuid :: String, suggestions :: Array String, search :: String }

data Message = Search String

type ComponentEff eff = (console :: CONSOLE, random :: RAND.RANDOM | eff )

component :: forall m. H.Component HH.HTML Query (Array String) Message (Aff (ComponentEff m))
component =
  H.lifecycleComponent
    { initialState: initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: HE.input UpdateSuggestions
    }
  where

  initialState :: (Array String) -> State
  initialState ss = { uuid: "", suggestions: ss, search: "" }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.datalist [HP.id_ state.uuid] (map (\x -> HH.option [x] []) values)
        , HH.input [ HP.value state.search
                     , HP.autocomplete false
                     , HP.required true
                     , HP.attr (H.AttrName "list") state.uuid
                     , HE.onValueInput (HE.input HandleInput)]
      ]
      where
        values = (map HP.value state.suggestions)

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (ComponentEff m))
  eval (HandleInput s next) = do
      state <- H.get
      do
        H.put {uuid: state.uuid, suggestions: state.suggestions, search: s}
        H.raise $ Search s
        pure next
  eval (Initialize next) = do
      state <- H.get
      r <- H.liftEff $ randomString 10 -- let's hope 10 is long enough to not clash in the real world
      H.put {uuid: r, suggestions: state.suggestions, search: state.search}
      pure next
  eval (UpdateSuggestions ss next) = do
      state <- H.get
      H.liftEff $ log $ show ss
      H.put {uuid: state.uuid, suggestions: ss, search: state.search}
      pure next

randomString :: forall m. Int -> Eff (console :: CONSOLE, random :: RAND.RANDOM | m ) String
randomString length = do
  rs <- sequence $ replicate length mkRnd
  pure $ trim $ fromCharArray $ fromFoldable $ map (fromCharCode) rs
  where
    mkRnd = (RAND.randomInt 97 122) -- 97 is ascii lower case 'a', 122 is 'z'
