module Main exposing (main)

import Box exposing (..)
import Lens exposing (..)
import Picture exposing (..)
import Fish exposing (fishShapes)
import Fitting exposing (createPicture)
import Html exposing (Html)
import Decor exposing (render)

main : Html msg
main = 
  let 
    box = { a = { x = 100.0, y = 50.0 }
          , b = { x = 400.0, y = 0.0 }
          , c = { x = 0.0, y = 400.0 } }
    lens = (box, Blackish)
    fish = createPicture fishShapes
  in     
    lens |> squareLimit 4 fish
         |> render [ ]
 