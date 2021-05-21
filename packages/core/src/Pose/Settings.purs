module Pose.Settings where

import Data.Maybe (Maybe(..))
import PureScript.CST.Types as CST

type Settings
  = { tabSize ∷ String
    , sourceStyle ∷ Maybe CST.SourceStyle
    }

defaultSettings ∷ Settings
defaultSettings = { tabSize: "  ", sourceStyle: Nothing }
