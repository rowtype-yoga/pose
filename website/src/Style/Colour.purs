module Style.Colour where

import Prelude
import Color (hsl, saturate)
import Plumage.Style.Color.Tailwind (blueGray, coolGray, warmGray)
import Record.Extra (mapRecord)

cream =
  { _50: hsl 38.0 0.3 0.97
  , _100: hsl 38.0 0.3 0.92
  , _200: hsl 34.0 0.3 0.89
  , _300: hsl 33.0 0.3 0.798
  , _400: hsl 33.0 0.3 0.722
  , _500: hsl 34.0 0.2 0.565
  , _600: hsl 50.0 0.33 0.285
  , _700: hsl 82.0 0.33 0.285
  , _800: hsl 110.0 0.339 0.225
  , _900: hsl 115.0 0.3 0.129
  }

primary =
  { _50: hsl 268.0 0.7 0.93
  , _100: hsl 268.0 0.7 0.821
  , _200: hsl 267.0 0.6 0.666
  , _300: hsl 267.0 0.6 0.618
  , _400: hsl 266.0 0.6 0.558
  , _500: hsl 266.0 0.75 0.59
  , _600: hsl 265.0 0.65 0.54
  , _700: hsl 265.0 0.68 0.4
  , _800: hsl 264.0 0.78 0.3
  , _900: hsl 264.0 0.91 0.2
  }

-- neutral = warmGray # mapRecord (saturate 0.05) -- cream
neutral = coolGray # mapRecord (saturate 0.05) -- cream
