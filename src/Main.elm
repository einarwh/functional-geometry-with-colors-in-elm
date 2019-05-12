module Main exposing (main)

import Box exposing (..)
import Lens exposing (..)
import Picture exposing (..)
import Fish exposing (fishShapes)
import Fitting exposing (createPicture)
import Rendering exposing (toSvg, toSvgWithBoxes)
import Svg exposing (Svg)
import Html exposing (Html)
import Decor exposing (decorate)

main : Html msg
main = 
  let 
    box = { a = { x = 100.0, y = 100.0 }
          , b = { x = 300.0, y = 0.0 }
          , c = { x = 0.0, y = 300.0 } }
    lens = (box, Blackish)
    fish = createPicture fishShapes
  in     
    lens |> fish 
         |> toSvgWithBoxes (500, 500) [ box ]
         |> decorate
 