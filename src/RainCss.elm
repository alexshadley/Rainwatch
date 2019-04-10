module RainCss exposing (..)

import Css exposing (..)

title : Style
title =
  Css.batch
    [ fontFamilies [ "Montserrat", "Arial" ]
    , fontSize (px 36)
    , fontStyle normal
    , textAlign center
    ]