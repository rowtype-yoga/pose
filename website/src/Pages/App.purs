module Pages.App (Props, mkApp) where

import Prelude hiding (div)
import Components.Editor.Types (MonacoEditorComponent)
import Components.Loading (mkLoading)
import Components.Page as Page
import Components.PageContainer (mkPageContainer)
import Context.Settings (mkSettingsProvider)
import Control.Monad.Reader (runReaderT)
import Data.Tuple.Nested ((/\))
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Next.Head as N
import React.Basic.DOM as R
import React.Basic.Hooks as React

type Props props
  = { "Component" ∷ Page.Component props
    , pageProps ∷ props
    }

mkApp ∷ ∀ props. MonacoEditorComponent → EffectFn1 (Props props) React.JSX
mkApp monacoEditorComponent =
  mkEffectFn1 \props → do
    context /\ settingsProvider ← mkSettingsProvider
    loading ← mkLoading
    let env = { settings: context, monacoEditorComponent }
    component ← runReaderT props."Component" env
    pageContainer ← mkPageContainer
    pure
      $ settingsProvider
      $ React.fragment
          [ N.head
              { children:
                  [ R.title
                      { children: [ R.text "Pose - PureScript Formatter" ]
                      }
                  ]
              }
          , loading unit
          , pageContainer [ component props.pageProps ]
          ]
