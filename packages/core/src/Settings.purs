module Settings where

import Prelude

import Data.Maybe (Maybe(..))
import PureScript.CST.Types as CST

type Settings = 
  { indentation :: String
  , sourceStyle :: Maybe CST.SourceStyle
  }

defaultSettings :: Settings
defaultSettings = { indentation: "  ", sourceStyle: Just CST.Unicode }