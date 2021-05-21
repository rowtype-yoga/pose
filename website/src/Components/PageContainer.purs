module Components.PageContainer where

import Prelude
import Effect (Effect)
import Plumage.Style.Global as Plumage
import React.Basic (JSX)
import React.Basic.Emotion as E
import React.Basic.Hooks (component)
import React.Basic.Hooks as React
import Yoga.Block as Block
import Yoga.Block.Container.Style (colour)

mkPageContainer ∷ Effect (Array JSX → JSX)
mkPageContainer = do
  component "Page Container" \children → React.do
    pure
      $ React.element
          Block.container
          { globalStyles:
              Plumage.globalStyles
                <> E.css
                    { "body":
                        E.nested
                          $ E.css
                              { "--mainFont": E.str "'ClashGroteskVariable', Comic Sans MS"
                              , minHeight: E.vh 100.0
                              , width: E.percent 100.0
                              , boxSizing: E.borderBox
                              , margin: E.str "0"
                              }
                    , "h1,h2,h3":
                        E.nested $ E.css { fontFamily: E.str "'ClashGroteskVariable', Comic Sans MS" }
                    , "p":
                        E.nested $ E.css { fontFamily: E.str "'SentientVariable', Comic Sans MS" }
                    }
          , children
          }
