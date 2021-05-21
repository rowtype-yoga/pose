module Env where

import Components.Editor.Types (MonacoEditorComponent)
import Context.Settings as Settings

type Env
  = { settings ∷ Settings.Context
    , monacoEditorComponent ∷ MonacoEditorComponent
    }
