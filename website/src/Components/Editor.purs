module Components.Editor where

import Prelude
import Color (toHexString)
import Color as Color
import Components.Editor.Types (MonacoEditorComponent)
import Control.Promise (Promise)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, notNull, null)
import Data.Time.Duration (Seconds(..))
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect (Effect)
import Effect.Aff.Compat (EffectFn1)
import Effect.Uncurried (EffectFn2, mkEffectFn1, mkEffectFn2)
import Foreign (Foreign, unsafeToForeign)
import Hooks.UseOnResize (useOnResize)
import Plumage.Style (mX', pB, pL, pT, pX)
import Plumage.Style.Border (roundedFull, roundedXl)
import Plumage.Style.BoxShadow (shadowXxl)
import Plumage.Style.Color.Background (background)
import Plumage.Style.Color.Tailwind (blueGray, coolGray, red)
import Plumage.Style.Color.Text (textCol)
import Plumage.Style.Text (textXl)
import Plumage.Util.HTML (div_, span_)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, Ref, element)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks (reactComponent, useEffectAlways)
import React.Basic.Hooks as React
import Style.Colour (neutral)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Node)
import Web.Event.Event (Event)
import Web.HTML (HTMLElement)
import Yoga.Block.Container.Style (DarkOrLightMode(..), getDarkOrLightMode)

type EditorProps
  = ( value ∷ String
    , onChange ∷ EffectFn2 String Event Unit
    , language ∷ String
    , editorDidMount ∷ EffectFn2 Editor Node Unit
    , editorWillMount ∷ EffectFn1 Monaco Unit
    , theme ∷ String
    , line ∷ Number
    , ref ∷ Ref (Nullable HTMLElement)
    , width ∷ String
    , height ∷ String
    , loading ∷ JSX
    , options ∷ Foreign
    )

foreign import defineThemeImpl ∷ Monaco → String → MonacoTheme → Effect Unit

foreign import setThemeImpl ∷ Monaco → String → Effect Unit

foreign import nightOwlTheme ∷ String → MonacoTheme

foreign import horizonTheme ∷ String → MonacoTheme

foreign import getValue ∷ Editor → Effect String

foreign import setValue ∷ String → Editor → Effect Unit

foreign import colorizeImpl ∷ String → String → { tabSize ∷ Int } → Editor → Promise String

foreign import layout ∷ Editor → Effect Unit

foreign import data Monaco ∷ Type

foreign import data Editor ∷ Type

foreign import data MonacoTheme ∷ Type

monacoEditor ∷ ∀ attrs attrs_. Union attrs attrs_ EditorProps ⇒ MonacoEditorComponent → ReactComponent { | attrs }
monacoEditor = unsafeCoerce

darkThemeName ∷ String
darkThemeName = "NightOwl"

lightThemeName ∷ String
lightThemeName = "Horizon"

foreign import data MonarchLanguage ∷ Type

foreign import purescriptSyntax ∷ MonarchLanguage

foreign import registerLanguageImpl ∷ Monaco → String → Effect Unit

foreign import setMonarchTokensProviderImpl ∷ Monaco → String → MonarchLanguage → Effect Unit

backgroundColDark ∷ String
backgroundColDark = "#172533FF"

backgroundColLight ∷ String
backgroundColLight = "#DFE4E8FF"

initEditor ∷ Monaco → Effect Unit
initEditor monaco = do
  defineThemeImpl monaco darkThemeName (nightOwlTheme backgroundColDark) -- [TODO] Read from somewhere else
  defineThemeImpl monaco lightThemeName (horizonTheme backgroundColLight) -- [TODO] Don't hardcode
  registerLanguageImpl monaco "purescript"
  setMonarchTokensProviderImpl monaco "purescript" purescriptSyntax

type Props
  = { onLoad ∷ Editor → Effect Unit
    , height ∷ String
    , language ∷ String
    , value ∷ String
    , onChange ∷ String → Event → Effect Unit
    }

mkEditor ∷ MonacoEditorComponent → Effect (ReactComponent Props)
mkEditor editorComponent = do
  reactComponent "Editor" \{ onChange, onLoad, height, language, value } → React.do
    monacoRef ← React.useRef null
    editorRef ← React.useRef null
    theme /\ setTheme ← React.useState' darkThemeName
    useOnResize (0.1 # Seconds) \_ → do
      mbEditor ← React.readRefMaybe editorRef
      for_ mbEditor layout
    useEffectAlways do
      mbMode ← getDarkOrLightMode
      case mbMode of
        Just DarkMode → setTheme darkThemeName
        Just LightMode → setTheme lightThemeName
        _ → mempty
      mempty
    pure
      $ element (monacoEditor editorComponent)
          { theme
          , height
          , value
          , onChange: mkEffectFn2 onChange
          , options:
              unsafeToForeign
                { fontFamily: "Jetbrains Mono"
                , fontLigatures: true
                , fontSize: "15px"
                , lineNumbers: "off"
                , glyphMargin: false
                , folding: false
                , lineDecorationsWidth: 0
                , lineNumbersMinChars: 0
                , minimap: { enabled: false }
                , scrollBeyondLastLine: false
                , hideCursorInOverviewRuler: true
                , overviewRulerBorder: false
                , renderLineHighlightOnlyWhenFocus: true
                , scrollbar: { alwaysConsumeMouseWheel: false }
                , renderLineHighlight: "none"
                }
          , language
          -- https://microsoft.github.io/monaco-editor/playground.html#extending-language-services-custom-languages
          , editorDidMount:
              mkEffectFn2 \e _ → do
                React.writeRef editorRef (notNull e)
                onLoad e
          , editorWillMount:
              mkEffectFn1 \m → do
                React.writeRef monacoRef (notNull m)
                initEditor m
          }
