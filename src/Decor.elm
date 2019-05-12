module Decor exposing (decorate)

import Svg exposing (Svg)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)

decorate : Svg msg -> Html msg 
decorate svg = 
  let 
    footer = 
      div [ style "color" "lightblue"
          , style "padding" "10px"
          , style "position" "fixed"
          , style "bottom" "0px"
          , style "zindex" "100"
          , style "width" "100%"
          , style "font-family" "Lucida Console"
          , style "font-size" "20px" ] 
          [ text "@einarwh" ] 
    body = div [ style "padding" "50px" ] [ svg ]
  in 
    div [] [ body, footer ]