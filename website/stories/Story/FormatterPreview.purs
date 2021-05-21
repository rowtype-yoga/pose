module Story.FormatterPreview (default, formatterPreview) where

import Prelude
import Components.Editor (mkEditor)
import Components.Editor.Types (MonacoEditorComponent)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import React.Basic (JSX, element, fragment)
import React.Basic.DOM as R
import React.Basic.Hooks (component)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

default ∷ { title ∷ String }
default = { title: "Ad Info" }

foreign import monacoEditorImpl ∷ ∀ attrs. Effect (Promise MonacoEditorComponent)

monacoEditor ∷ Aff MonacoEditorComponent
monacoEditor = do
  prom ← monacoEditorImpl # liftEffect
  Promise.toAff prom

formatterPreview ∷ Effect JSX
formatterPreview = ado
  editor ← mkEditorExample
  in editor unit

mkEditorExample ∷ Effect (Unit → JSX)
mkEditorExample =
  component "Editor Example" \props → React.do
    maybeEditor /\ setEditor ← React.useState' Nothing
    useAff unit do
      eddy ← monacoEditor
      editor ← liftEffect $ mkEditor eddy
      liftEffect (setEditor (Just editor))
    pure
      ( maybeEditor
          # foldMap \editor →
              React.element editor
                { onLoad: const (pure unit)
                , height: "100px"
                , language: "purescript"
                , value: "module Main where"
                , onChange: \string event → pure unit
                }
      )
