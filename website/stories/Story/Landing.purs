module Story.Landing (default, landing) where

import Prelude
import Components.Editor (mkEditor)
import Components.Editor.Types (MonacoEditorComponent)
import Components.PageContainer (mkPageContainer)
import Context.Settings (mkSettingsProvider)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Trans (lift)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Pages.App (mkApp)
import Pages.Landing (mkLanding)
import React.Basic (JSX, element, fragment)
import React.Basic.DOM as R
import React.Basic.Hooks (component)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

default ∷ { title ∷ String, decorators ∷ Array (Effect JSX → JSX) }
default =
  { title: "Landing"
  , decorators:
      [ \mkStory →
          unsafePerformEffect ado
            story ← mkStory
            pageContainer ← mkPageContainer
            in pageContainer [ story ]
      ]
  }

foreign import monacoEditorImpl ∷ Effect (Promise MonacoEditorComponent)

monacoEditor ∷ Aff MonacoEditorComponent
monacoEditor = do
  prom ← monacoEditorImpl # liftEffect
  Promise.toAff prom

landing ∷ Effect JSX
landing = do
  mkLandingExample <@> unit

mkLandingExample ∷ Effect (Unit → JSX)
mkLandingExample = do
  context /\ settingsProvider ← mkSettingsProvider
  component "Editor Example" \props → React.do
    maybeLanding /\ setLanding ← React.useState' Nothing
    useAff unit do
      eddy ← monacoEditor
      landing ← liftEffect $ runReaderT mkLanding { monacoEditorComponent: eddy, settings: context }
      liftEffect (setLanding (Just landing))
    pure
      (maybeLanding # foldMap \landing → landing { header: "Eddy" })
