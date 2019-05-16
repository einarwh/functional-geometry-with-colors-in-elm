module Decor exposing (render)

import Box exposing (Box)
import Picture exposing (Rendering)
import Svg exposing (Svg)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Rendering exposing (toSvg, toSvgWithBoxes)

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
    div [ ] [ body, footer ]

render : List Box -> Rendering -> Html msg 
render boxes rendering = 
  let 
    bounds = (500, 500)
  in 
    rendering 
    |> toSvgWithBoxes bounds boxes
    |> decorate  
