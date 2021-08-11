module Pages.Landing (Props, mkLanding, getServerSideProps) where

import Prelude hiding (div)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Color (black, white, cssStringRGBA)
import Color as Number
import Components.Editor (mkEditor)
import Components.Page as Page
import Config as Config
import Control.Promise (Promise, fromAff)
import Data.Array ((..))
import Data.Either (Either(..), either)
import Data.Foldable (foldMap)
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Monoid (power)
import Data.Number (infinity)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.TwoOrMore (TwoOrMore, twoOrMore)
import Data.TwoOrMore as TwoOrMore
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Framer.Motion as M
import Plumage.Atom.Button (mkButton, primaryButtonStyle)
import Plumage.Layer (zIndex)
import Plumage.Layer as Layer
import Plumage.Style (mB, mR, mT, mX', mXAuto, mY, pB, pL, pR, pT, pX, pXY, pY)
import Plumage.Style.Border (border, borderCol, borderSolid, rounded, rounded2xl, roundedDefault, roundedSm, roundedXl)
import Plumage.Style.BoxShadow (shadowDefault, shadowLg, shadowSm, shadowXxl)
import Plumage.Style.Breakpoint (screenSm)
import Plumage.Style.Color.Background (background, linearGradient)
import Plumage.Style.Color.Tailwind (amber, gray, lime, pink, purple, rose, teal, violet)
import Plumage.Style.Color.Text (textCenter, textCol)
import Plumage.Style.Color.Util (withAlpha)
import Plumage.Style.Cursor (cursorMove, cursorPointer)
import Plumage.Style.Display.Flex (flexCol, flexRow, gap, itemsCenter, justifyBetween, justifyCenter, justifyEnd)
import Plumage.Style.Divide (divideX)
import Plumage.Style.Opacity (opacity)
import Plumage.Style.Size (full, height, height', maxWidth, screenWidth, width, width', widthFull, widthScreen)
import Plumage.Style.Text (fontBlack, fontBold, fontLight, fontMedium, fontNormal, fontSemibold, text2xl, text3xl, text4xl, text6xl, text7xl, textBase, textLg, textSm, textXl, textXs, trackingNormal, trackingTight, trackingTighter, trackingWide, trackingWider, trackingWidest)
import Plumage.Util.HTML as H
import Pose.Format (format)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types as SourceStyle
import React.Basic (JSX, ReactComponent)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.SVG as SVG
import React.Basic.Emotion as E
import React.Basic.Events (handler_, handler)
import React.Basic.Hooks as React
import Style.Colour (neutral, primary)
import Yoga ((/>), (</), (</*))
import Yoga.Block as Block
import Yoga.Block.Atom.Button.Types as ButtonType
import Yoga.Block.Atom.Range.Style (thumbStyle)
import Yoga.Block.Container.Style (colour)

type Props
  = { header ∷ String
    }

examples ∷ TwoOrMore ({ id ∷ String, value ∷ String } /\ String)
examples =
  twoOrMore
    ({ id: "your-own", value: "Your Own" } /\ "")
    ({ id: "functions", value: "Functions" } /\ functionExample)
    [ { id: "comments", value: "Comments" } /\ commentsExample
    ]

mkLanding ∷ Page.Component Props
mkLanding = do
  -- button ← mkButton # lift
  Page.component "Home" \env props → React.do
    (maybeEditor ∷ Maybe (ReactComponent _)) /\ setEditor ← React.useState' Nothing
    editorLoaded /\ setEditorLoaded ← React.useState' false
    { index: activeExampleIndex, example } /\ setActiveExample ← React.useState' { index: 1, example: TwoOrMore.head examples }
    (editorText ∷ String) /\ setEditorText ← React.useState' functionExample
    formatterSettings /\ updateFormatterSettings ← React.useState { tabSize: 2, codeStyle: Nothing }
    textBeforeFormat /\ setTextBeforeFormat ← React.useState' Nothing
    let
      indentationSliderWidth = 160 # E.px
      indentationSliderRange =
        React.element Block.range
          { css:
              width' indentationSliderWidth
                <> height' (18 # E.px)
                <> E.css
                    { "input[type=range]::-webkit-slider-thumb": E.nested (thumbStyle <> E.css { transform: E.str "translate(0, -1px)" })
                    , "input[type=range]::-moz-range-thumb": E.nested (thumbStyle <> E.css { transform: E.str "translate(0, -1px)" })
                    }
          , min: 0
          , max: 6
          , value: formatterSettings.tabSize - 2
          , onChange:
              handler targetValue \x → case x >>= Int.fromString of
                Just n → updateFormatterSettings (_ { tabSize = n + 2 })
                _ → mempty
          }
      indentationSliderNumberLabel =
        H.div_
          ( width' indentationSliderWidth
              <> flexRow
              <> justifyBetween
              <> textXs
              <> pL 4
              <> pR 3
          )
          $ (2 .. 8)
          <#> \n →
              ( if formatterSettings.tabSize == n then
                  H.div_
                    ( fontSemibold
                        <> E.css
                            { color: E.str colour.highlightText
                            , position: E.relative
                            , "&::before":
                                E.nested
                                  $ E.css
                                      { background: E.str colour.highlight
                                      , content: E.str $ "'" <> show n <> "'"
                                      , position: E.absolute
                                      , textAlign: E.center
                                      , left: E.str $ show ((-3.0) - (Int.toNumber n * 0.4)) <> "px" -- Crazy
                                      , top: E.px (-4)
                                      }
                                  <> rounded full
                                  <> shadowSm
                                  <> pT 1
                                  <> width 18
                                  <> height 18
                            }
                    )
                    [ R.text $ show n ]
                else
                  H.div_
                    ( E.css
                        { color: E.str colour.textPaler4
                        , userSelect: E.none
                        }
                        <> cursorPointer
                    )
                    [ R.span
                        { onClick: handler_ $ updateFormatterSettings (_ { tabSize = n })
                        , children: [ R.text $ show n ]
                        }
                    ]
              )
      indentationSliderLabel =
        H.div_
          ( E.css { color: E.str colour.textPaler2 }
              <> trackingWide
              <> textXs
              <> fontLight
              <> mB 12
              <> mT 8
          )
          [ R.text "Indentation" ]
      indentationSlider =
        H.div_ (flexCol)
          [ indentationSliderLabel, indentationSliderNumberLabel, indentationSliderRange ]
      poseSettings =
        { tabSize: power " " formatterSettings.tabSize
        , sourceStyle: formatterSettings.codeStyle
        }
    React.useEffectOnce do
      (setEditor <<< Just <=< mkEditor) env.monacoEditorComponent
      mempty
    settings ← React.useContext env.settings
    React.useEffect settings do
      Console.log $ fromMaybe "No settings" settings
      mempty
    let
      undoButton =
        textBeforeFormat
          # foldMap \textBefore →
              H.div_ (flexRow <> justifyEnd)
                [ Block.button
                    { onClick:
                        handler_ do
                          setEditorText textBefore
                          setTextBeforeFormat Nothing
                    , buttonType: ButtonType.Generic
                    }
                    [ H.span_ (pX 8) [ R.text "Unformat" ] ]
                ]
      formatButton =
        H.div_ (flexRow <> justifyEnd)
          [ Block.button
              { onClick:
                  handler_ case parseModule editorText of
                    ParseSucceeded pf → do
                      setTextBeforeFormat (Just editorText)
                      setEditorText $ format poseSettings pf
                    _ → Console.error "Failed to parse"
              , buttonType: ButtonType.Primary
              }
              [ H.span_ (pX 16 <> fontMedium <> textLg <> trackingWide)
                  [ R.text "Format" ]
              ]
          ]
      codeStyle =
        H.div_ (flexCol)
          [ codeStyleLabel
          , codeStyleButtonGroup
          ]
      codeStyleLabel =
        H.div_
          (E.css { color: E.str colour.textPaler2 } <> fontNormal <> trackingWide <> textXs <> fontLight <> mB 12 <> mT 8)
          [ R.text "Code Style" ]
      codeStyleButtonGroup =
        H.div_
          ( flexRow
          )
          [ Block.button
              { css:
                  E.css { borderRadius: E.str "var(--s-1) 0 0 var(--s-1)" }
                    <> baseStyle
                    <> asciiButtonStyle
              , onClick:
                  handler_ case formatterSettings.codeStyle of
                    Just SourceStyle.ASCII → updateFormatterSettings (_ { codeStyle = Nothing })
                    _ → updateFormatterSettings (_ { codeStyle = Just SourceStyle.ASCII })
              }
              [ H.span_
                  ( case formatterSettings.codeStyle of
                      Just SourceStyle.ASCII → E.css { fontWeight: E.str "660" }
                      Just SourceStyle.Unicode → E.css { color: E.str colour.textPaler4 }
                      Nothing → E.css { color: E.str colour.textPaler3 }
                  )
                  [ R.text "ASCII" ]
              ]
          , H.div_
              ( E.css
                  { background: E.str colour.backgroundLayer3
                  , width: E.px 1
                  , height: E.percent 100.0
                  }
              )
              []
          , Block.button
              { css:
                  E.css { borderRadius: E.str "0 var(--s-1) var(--s-1) 0" }
                    <> baseStyle
                    <> unicodeButtonStyle
              , onClick:
                  handler_ case formatterSettings.codeStyle of
                    Just SourceStyle.Unicode → updateFormatterSettings (_ { codeStyle = Nothing })
                    _ → updateFormatterSettings (_ { codeStyle = Just SourceStyle.Unicode })
              }
              [ H.span_
                  ( case formatterSettings.codeStyle of
                      Just SourceStyle.Unicode → E.css { fontWeight: E.str "660" }
                      Just SourceStyle.ASCII → E.css { color: E.str colour.textPaler4 }
                      Nothing → E.css { color: E.str colour.textPaler3 }
                  )
                  [ R.text "Unicode" ]
              ]
          ]
        where
        baseStyle = shadowSm <> height 30 <> width 80 <> E.css { color: E.str colour.textPaler1 }
        pressedStyle =
          E.css
            { boxShadow: E.str "inset -1px -2px 4px rgba(0,0,0,0.4)"
            , borderColor: E.str $ colour.highlight
            , background: E.str $ colour.highlight
            , "& > *":
                E.nested
                  $ E.css
                      { color: E.str colour.highlightText
                      , fontWeight: E.str "620"
                      }
            }
        (asciiButtonStyle /\ unicodeButtonStyle) = case formatterSettings.codeStyle of
          Nothing → mempty /\ mempty
          Just SourceStyle.ASCII → pressedStyle /\ mempty
          Just SourceStyle.Unicode → mempty /\ pressedStyle

      theEditor =
        H.div_
          ( E.css
              { position: E.absolute
              , top: E.px 0
              , left: E.str "0"
              }
              <> widthScreen
              <> Layer.level3
              <> pB 20
              <> pT 280
          )
          [ H.div_
              ( mX' E.auto
                  <> pT 16
                  <> pB 20
                  <> pL 24
                  <> shadowLg
                  <> mXAuto
                  <> opacity 90
                  <> E.css
                      { width: E.vw 100.0
                      , background: E.str colour.backgroundLayer2
                      , backdropFilter: E.str "blur(10px)"
                      , "@media (min-width: 900px)":
                          E.nested
                            $ ( E.css { width: E.px 800 }
                                  <> roundedXl
                              )
                      , left: E.px 0
                      }
              )
              [ H.div_
                  ( flexRow
                      <> justifyCenter
                      <> pB 16
                      <> E.css { fontFamily: E.str "ClashGroteskVariable" }
                  )
                  [ Block.segmented
                      { buttonContents: examples <#> fst
                      , activeIndex: activeExampleIndex
                      , updateActiveItem:
                          \_ index →
                            when (index /= activeExampleIndex) do
                              let
                                newExample =
                                  examples
                                    TwoOrMore.!! index
                                    # fromMaybe' \_ → TwoOrMore.head examples
                              setTextBeforeFormat Nothing
                              setActiveExample { example: newExample, index }
                              setEditorText (snd newExample)
                      }
                  ]
              , case editorLoaded, maybeEditor of
                  true, Just editor →
                    React.elementKeyed editor
                      { key: "eddy"
                      , onLoad: \_ → setEditorLoaded true
                      , height: "300"
                      , language: "purescript"
                      , value: editorText
                      , onChange:
                          \s _ → do
                            when (Just s /= textBeforeFormat) $ setTextBeforeFormat Nothing
                            setEditorText s
                      }
                  false, Just editor →
                    H.div "editor-skeleton"
                      (height 300 <> widthFull <> E.css { background: E.str colour.backgroundLayer5 })
                      [ React.elementKeyed editor
                          { key: "eddy"
                          , onLoad: \_ → setEditorLoaded true
                          , height: "300"
                          , language: "purescript"
                          , value: editorText
                          , onChange: \s _ → setEditorText s
                          }
                      ]
                  _, _ →
                    H.div "editor-skeleton"
                      (height 300 <> widthFull <> E.css { background: E.str colour.background })
                      []
              , E.element R.hr'
                  { className: ""
                  , css:
                      ( E.css { position: E.relative, borderTop: E.px 1 }
                          <> borderSolid
                          <> width' (E.str "calc(100% - 24px)")
                          <> E.css { borderColor: E.str colour.backgroundLayer1 }
                          <> mR 440
                      )
                  }
              , Block.cluster { justify: "space-between", align: "flex-end", css: pR 24 <> pT 12 }
                  [ R.div_
                      [ H.span_
                          ( textXs
                              <> fontMedium
                              <> trackingWider
                              <> E.css { alignSelf: E.str "top" }
                              <> E.css { color: E.str colour.textPaler2 }
                              <> mB 12
                          )
                          [ R.text "SETTINGS" ]
                      , Block.cluster { space: "var(--s2)", align: "flex-start" }
                          [ indentationSlider
                          , codeStyle
                          ]
                      ]
                  , Block.cluster { space: "var(--s-1)" }
                      [ undoButton
                      , formatButton
                      ]
                  ]
              ]
          ]
    pure
      $ H.div "landing-page"
          ( pT 120
              <> pB 80
              <> widthFull
              <> E.css
                  { background: E.str colour.backgroundLayer4
                  , overflowX: E.hidden
                  , minHeight: E.vh 100.0
                  }
          )
          [ githubLink
          , H.div_
              ( E.css
                  { position: E.absolute
                  , left: E.px 0
                  , top: E.px 0
                  , overflowX: E.hidden
                  , pointerEvents: E.none
                  }
                  <> pT 100
                  <> widthFull
                  <> Layer.base
              )
              [ H.div_
                  ( E.css { position: E.relative, left: E.str "50%", transform: E.str "translate(270px, 120px)" }
                      <> height 400
                      <> width 400
                  )
                  [ blob ]
              , H.div_
                  ( E.css { position: E.relative, left: E.str "50%", transform: E.str "translate(-700px, -100px)" }
                      <> height 400
                      <> width 400
                  )
                  [ blob2 ]
              , H.div_
                  ( E.css { position: E.relative, left: E.str "50%", transform: E.str "translate(-1200px, -800px)" }
                      <> height 800
                      <> width 800
                  )
                  [ blob3 ]
              ]
          , H.div_ (flexCol <> justifyCenter <> itemsCenter <> gap 48)
              [ R.h1'
                  </* { className: "", css: (text7xl <> fontSemibold <> trackingTight <> E.css { color: E.str colour.textPaler3 }) }
                  /> poseAnimation
              , R.h2' </* { css: (text3xl <> fontNormal <> trackingWide <> E.css { color: E.str colour.textPaler2 }) } /> [ R.text "A formatter for PureScript" ]
              ]
          , R.noscript_ [ R.text "Turn on JavaScript to try out POSE live in your browser now" ]
          , theEditor
          , R.div'
              </* { css: pX 8 <> screenSm (pX 24) <> maxWidth 900 <> mXAuto <> mT 700 }
              />
                [ mainHeading "User Manual"
                , stepHeading "Installation"
                , R.p_ [ R.text "We hook into prettier to take care of a lot of the heavy lifting for us so let's install it and the plugin:" ]
                , codeSnippet "npm install --save-dev prettier \\\n  @rowtype-yoga/prettier-plugin-purescript"
                , R.p_ [ R.text "We only want to format our own code so we modify ", monospan ".prettierignore", R.text ":" ]
                , codeSnippet """echo -e ".spago\noutput/" >> .prettierignore"""
                , stepHeading "Formatting"
                , R.p_ [ R.text "If you use VSCode then you can simply turn on format on save and you're good to go, if you want to manually format some files here's an example:" ]
                , codeSnippet """npx prettier --write src"""
                , stepHeading "Configuring"
                , R.p_ [ R.text "You can configure the unicodeness and the indentation depth of your code in ", monospan ".prettierrc" ]
                , codeSnippet
                    """{
  "tabWidth": 2,
  "pureScriptSymbolStyle": "unicode"
}
"""
                , R.p_ [ R.text "" ]
                , mainHeading "Acknowledgements"
                , R.p_ [ R.text "In closing we would like to thank Hardy Jones and Nate Faubion, we could not have built pose without purty and the purescript cst-parser!" ]
                ]
          ]

githubLink =
  R.a'
    </*
      { css:
          E.css
            { position: E.absolute
            , top: E.str "24px"
            , right: E.str "24px"
            , textDecoration: E.str "underline"
            }
            <> fontLight
            <> Layer.level4
            <> textSm
      , href: "https://github.com/rowtype-yoga/pose"
      }
    /> [ R.text "Github Repo" ]

monospan :: String -> JSX
monospan text =
  R.code'
    </*
      { css:
          E.css
            { fontFamily: E.str "Times'Jetbrains Mono', ui-monospace, monospace" }
      }
    /> [ R.text text ]

stepHeading :: String -> JSX
stepHeading text =
  R.h3'
    </*
      { css: (text2xl <> screenSm text4xl <> fontSemibold <> trackingNormal <> E.css { color: E.str colour.textPaler3 } <> pY 12)
      }
    /> [ R.text text ]

mainHeading :: String -> JSX
mainHeading text =
  R.h2'
    </*
      { css: (text4xl <> screenSm text6xl <> fontSemibold <> textCenter <> trackingTight <> E.css { color: E.str colour.textPaler3, hyphens: E.auto, wordWrap: E.str "break-word" } <> pY 36 <> mT 16)
      }
    /> [ R.text text ]

codeSnippet :: String -> JSX
codeSnippet text =
  R.div'
    </*
      { css:
          background (gray._900 # withAlpha 0.9)
            <> textCol gray._400
            <> roundedDefault
            <> screenSm (rounded2xl)
            <> shadowLg
            <> mXAuto
            <> maxWidth 600
            <> mY 40
            <> textXs
            <> screenSm textSm
      }
    />
      [ R.pre'
          </*
            { css:
                pX 8
                  <> pY 10
                  <> screenSm (pX 24 <> pY 20)
                  <> E.css { overflow: E.auto }
            }
          />
            [ R.code'
                </* { css: E.css { fontFamily: E.str "'Jetbrains Mono', monospace" } }
                /> [ R.text text ]
            ]
      ]

poseAnimation ∷ Array JSX
poseAnimation =
  [ M.div
      </*
        { initial: M.initial (R.css { y: -20.0, x: -20, rotate: 10.0 })
        , animate: M.animate (R.css { y: 0, x: 0, rotate: 0, transition: { delay: 0.2 } })
        , className: ""
        , css: E.css { display: E.inlineBlock }
        }
      /> [ R.text "P" ]
  , M.div
      </*
        { initial: M.initial (R.css { y: 20.0, rotate: -10 })
        , animate: M.animate (R.css { y: 0.0, rotate: 0, transition: { delay: 0.4 } })
        , className: ""
        , css: E.css { display: E.inlineBlock }
        }
      /> [ R.text "O" ]
  , M.div
      </*
        { initial: M.initial (R.css { y: 20, x: 10 })
        , animate: M.animate (R.css { y: 0, x: 0, transition: { delay: 0.0 } })
        , className: ""
        , css: E.css { display: E.inlineBlock }
        }
      /> [ R.text "S" ]
  , M.div
      </*
        { initial: M.initial (R.css { x: 10, scaleX: -1, originX: "50%" })
        , animate: M.animate (R.css { x: 0, scaleX: 1, transition: { delay: 0.4 } })
        , className: ""
        , css: E.css { display: E.inlineBlock }
        }
      /> [ R.text "E" ]
  ]

fetchData ∷ ∀ ctx. ctx → Aff Props
fetchData _ = do
  res ← AX.request (AX.defaultRequest { url = Config.apiEndpoint <> "/posts/1", method = Left GET, responseFormat = ResponseFormat.string })
  liftEffect $ Console.log $ either AX.printError _.body res
  pure $ { header: "Home" }

getServerSideProps ∷ ∀ ctx. EffectFn1 ctx (Promise { props ∷ Props })
getServerSideProps =
  mkEffectFn1
    $ fromAff
    <<< map { props: _ }
    <<< fetchData

mkBlob col d =
  SVG.svg
    { viewBox: "0 0 200 200"
    , children:
        [ SVG.path
            { d
            , transform: "translate(100 100)"
            , fill: cssStringRGBA col
            }
        ]
    }

blob =
  mkBlob teal._200
    "M40.7,-38.7C55.4,-26,71.9,-13,74.7,2.8C77.5,18.5,66.5,37.1,51.8,50.5C37.1,63.9,18.5,72.1,-1.9,74C-22.3,75.9,-44.6,71.4,-52.3,58C-60.1,44.6,-53.4,22.3,-52,1.3C-50.7,-19.6,-54.7,-39.2,-47,-52C-39.2,-64.7,-19.6,-70.7,-3.3,-67.3C13,-64,26,-51.5,40.7,-38.7Z"

blob2 = mkBlob pink._300 "M47.6,-42.9C63.9,-31.2,81.1,-15.6,80.5,-0.7C79.8,14.3,61.3,28.5,44.9,41.8C28.5,55.2,14.3,67.6,0.1,67.5C-14.1,67.5,-28.3,54.9,-35.7,41.6C-43.2,28.3,-43.9,14.1,-41.8,2.1C-39.7,-9.9,-34.6,-19.7,-27.2,-31.5C-19.7,-43.2,-9.9,-56.8,2.9,-59.7C15.6,-62.5,31.2,-54.6,47.6,-42.9Z"

blob3 = mkBlob amber._300 "M46.6,-67.3C59.4,-54.8,68.1,-39.9,69.7,-25C71.2,-10.2,65.5,4.6,61.4,20.8C57.3,37,54.9,54.6,44.9,63.4C35,72.1,17.5,72.1,1.4,70.2C-14.7,68.2,-29.4,64.5,-44.9,57.5C-60.4,50.6,-76.6,40.4,-81.7,26.2C-86.7,12.1,-80.5,-6,-72.7,-21.7C-65,-37.4,-55.7,-50.6,-43.3,-63.2C-30.9,-75.8,-15.5,-87.9,0.7,-88.8C16.9,-89.8,33.8,-79.8,46.6,-67.3Z"

functionExample =
  """module Functions where

import Prelude

someFunction      a  b   =      a           +   b

someFunctionWithNewline a    b   =
         a      +         b

someMultiLineFunction a b =
  -- apply a function
            someFunction
                a
                  b

aRecordFunction {
  a,
       b
} =       a     + b

aDeconstructedFunction blurb@{ a,
  b
} = a + b

aFunctionWithSignature ::         Int ->        Int
aFunctionWithSignature = identity

aFunctionWithForallSignature :: forall a. a -> a
aFunctionWithSignature = identity

aFunctionWithForallButOneLine ::
  forall a. a -> a
aFunctionWithForallButOneLine = identity

aFunctionWithForallButTwoLines ::
  forall a.
  a -> a
aFunctionWithForallButTwoLines = identity

getName ::
  forall a.
  Eq a =>
  String -> a -> String
getName foo a = foo

getNameLine ::
  String -> Int -> String -> Boolean
getNameLine foo a bar = true

getNameOne :: String -> Int -> String
getNameOne foo a = foo

"""

commentsExample =
  """-- These are some comments.
{- Here's a block comment.
It has multiple lines.
-}
module ModuleComments where

-- This comment is on an import
import Prelude

{- A comment on a class -}
class Foo

-- Two comments
-- one class
class
  Foo

-- Two comments
-- on a more complicated class
class Unfoldable1 t where
  unfoldr1 :: forall a b. (b -> Tuple a (Maybe b)) -> b -> t a

-- A Block comment on a data.
data X
  = X -- A line comment on a constructor
  | X2 {- A block comment on a construction -}         {-Another block comment on the same constructor-}
  | X3      -- A lot of spaces

{- | A Line comment on a newtype -}
newtype Y
  = Y X

-- A Line comment on a fixity
infix 0 y as !

{- A Block comment on a foreign -}
foreign import kind Bar

-- A Line comment on an instance
instance foo ::
  Foo

{- A Block comment on a synonym -}
type Z
  = Y

-- | Another line comment.
x = X

{- A block comment on a type -}
y :: Y -> Y -> Y
y _ _ = Y

-- A comment should not make the next line indent so it's invalid
type MyRow :: Symbol -> Row Type
type MyRow s = s

-- A comment should not make the next line indent so it's invalid
type MyRow ::
  Symbol -> Row Type
type MyRow s = s

fun :: Int
fun =
  identity
    -- please don't put
    -- newlines
    -- in between these
    $ 0

commentGluing :: Int
commentGluing =
  identity
    $ -- leave these comments
      -- on separate lines
      0

commentGluingWorse :: Int
commentGluingWorse =
  identity
    $ -- don't turn the next line into a comment
      0


commentInRecord =
  { -- don't destroy the code below
  x: 12
  }

commentInRecord2 =
  { y: 12
  , -- don't destroy the code below either
  x: 12
  }

commentInRecord3 =
  { y: 12
  ,
  x: 12 -- don't destroy the code below either
  , z: 8
  }

commentInRecord4 =
  { y: 12
  ,
  x: 12 -- don't destroy the closing curly
  }

commentInRecord5 =
  { y: 12
  ,
  x: 12
  -- don't destroy the closing curly
  }

commentInArray =
  [ -- don't destroy the code below
  12
  ]

commentInArray2 =
  [ 12
  , -- don't destroy the code below either
  12
  ]

commentInArray3 =
  [  12
  ,
  12 -- don't destroy the code below either
  ,  8
  ]

commentedOut = do
  -- (a /\ b)
  (a /\ b) ← pure ("1" /\ "2")
  -- x /\ y
  x /\ y ← pure ("1" /\ "2")
  -- c /\ d
  c /\ d ←
    pure
      ("c" /\ "d")
  pure unit

span = go Nothing
  where
  go i =
    -- comment one
    -- comment 2
    case i of
      -- Everything must be commented
      Just x -> 1 -- here too, why not?
      -- Sometimes even in
      -- greater detail
      Nothing  -- A comment would be good
        -> -- here
        -- what about here?
        2 -- and here?

inlineComments :: IAm -- line comment
  -> Boolean {- block comment -}
  -> Boolean ->
  -- ^ comment after arrow
  Int
  -> BeforeAnArrow
inlineComments = 77

inlineCommentsParens -- inline comments
  :: -- this is a double colon
  forall a b m -- for all of them
  . Eq a -- equals?
  => Ord -- ordered?
    a =>
  (e -> Maybe b) -- ^ I really like this
  -> m a -- ^ I really like this, too
  -> { a :: Int } -- ^ And this
  -> Maybe b -- ^ I really like this, too
  -> _ -- ^ I really like this, too
  -> m _ -- Oh no, a hole
  -> Int
inlineCommentsParens = 1

nested =
  insert (Tuple "1" "2")
  where
  insertSep state@(Tuple stk acc) = case tpl of
    -- Comment 1
    Tuple lytPos LytTopDecl | sepP lytPos ->
      1
    -- Comment 2 3
    Tuple what a : b ->
      2
    -- Comment with more lines
    Tuple what a
      : b -> 3

-- A trailing comment on the module
-- Another trailing module comment


{- A trailing block comment on the module -}
{- One more
  trailing block comment
        across multiple lines
-}
-- | And another trailing line comment
"""
