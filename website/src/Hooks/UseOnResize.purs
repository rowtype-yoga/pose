module Hooks.UseOnResize where

import Prelude

import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Time.Duration (class Duration)
import Data.Time.Duration as Milliseconds
import Effect (Effect)
import Effect.Aff (Aff, Fiber, delay, error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Math as Math
import React.Basic.Hooks (Hook, UseLayoutEffect, UseState, coerceHook, useLayoutEffect, useLayoutEffectOnce, (/\))
import React.Basic.Hooks as React
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventListener, EventTarget, addEventListener, eventListener, removeEventListener)
import Web.HTML as HTML
import Web.HTML.Window as Window

eventType ∷ EventType
eventType = EventType "resize"

registerListener ∷ EventListener -> Effect (Effect Unit)
registerListener listener = do
  target <- HTML.window <#> Window.toEventTarget
  addEventListener eventType listener false target
  pure $ removeEventListener eventType listener false target

type Sizes =
  { innerWidth ∷ Number, innerHeight ∷ Number }

newtype UseResize hooks = UseResize (UseLayoutEffect Unit (UseState Sizes hooks))

derive instance ntUseResize ∷ Newtype (UseResize hooks) _

useResize ∷ Hook UseResize Sizes
useResize =
  coerceHook React.do
    size /\ setSize <- React.useState' zero
    useLayoutEffect unit do
      setSizeFromWindow setSize
      listener <- makeListener setSize
      registerListener listener
    pure size

setSizeFromWindow ∷ (Sizes -> Effect Unit) -> Effect Unit
setSizeFromWindow setSize = do
  window <- HTML.window
  innerWidth <- Window.innerWidth window <#> toNumber
  innerHeight <- Window.innerHeight window <#> toNumber
  setSize { innerWidth, innerHeight }

makeListener ∷ (Sizes -> Effect Unit) -> Effect EventListener
makeListener setSize = do
  eventListener
    $ const (setSizeFromWindow setSize)

newtype UseOnResize hooks = UseOnResize
  ( UseLayoutEffect Unit
      ( UseState Sizes
          (UseState (Maybe (Fiber Unit)) hooks)
      )
  )

derive instance ntUseOnResize ∷ Newtype (UseOnResize hooks) _

useOnResize ∷
  ∀ d.
  Duration d =>
  d ->
  ({ innerWidth ∷ Number, innerHeight ∷ Number, deltaWidth ∷ Number, deltaHeight ∷ Number } -> Effect Unit) ->
  Hook UseOnResize Unit
useOnResize debounceBy callback =
  coerceHook React.do
    mbFiber /\ setFiber <- React.useState' Nothing
    size /\ setSize <- React.useState' (zero ∷ Sizes)
    let
      layoutEffect ∷ Effect (Effect Unit)
      layoutEffect = do
        setSizeFromWindow (setSize ∷ Sizes -> Effect Unit)
        listener ∷ EventListener <-
          makeListener \(dimensions ∷ Sizes) -> do
            let
              aff ∷ Aff Unit
              aff = do
                for_ mbFiber (killFiber (error "Fiber cancelled"))
                delay (Milliseconds.fromDuration debounceBy)
                let { innerWidth, innerHeight } = dimensions
                let deltaWidth = Math.abs (size.innerWidth - innerWidth)
                let deltaHeight = Math.abs (size.innerHeight - innerWidth)
                setSize dimensions # liftEffect
                callback { innerWidth, innerHeight, deltaWidth, deltaHeight } # liftEffect
            fiber ∷ Fiber _ <- launchAff aff
            setFiber (Just fiber)
        target ∷ EventTarget <- HTML.window <#> Window.toEventTarget
        addEventListener eventType listener false target
        pure
          $ launchAff_
          $ for_ mbFiber (killFiber (error "Fiber cancelled"))
    useLayoutEffectOnce layoutEffect
