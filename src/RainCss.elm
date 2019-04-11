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

chartContainer : Style
chartContainer =
  Css.batch
    [ width (pct 50) 
    , margin auto
    ]