module Main where

import Prelude
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random as RAND
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Network.HTTP.Affjax as AX

import Halogen.Components.MapboxPlaces (component)

main :: Eff (HA.HalogenEffects (console :: CONSOLE, random :: RAND.RANDOM, ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component "my-key" body
