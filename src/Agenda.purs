module Agenda where

import Prelude
import Data.Generic
import Data.DateTime
import Data.Maybe
import Data.Enum

import Control.Applicative

data Activity = Activity { summary :: String
                         , start :: DateTime
                         , end :: DateTime }

mkActivity :: String -> DateTime -> DateTime -> Activity
mkActivity summary start end =
  Activity { summary: summary
           , start: start
           , end: end }

derive instance genericActivity :: Generic Activity

instance showActivity :: Show Activity where
  show = gShow

instance eqActivity :: Eq Activity where
  eq = gEq
