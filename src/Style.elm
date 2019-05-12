module Style exposing (..)

type StyleColor = B | G | W

type alias StrokeStyle = 
  { strokeWidth : Float 
  , strokeColor : StyleColor }

type alias FillStyle = 
  { fillColor : StyleColor }

type alias Style = 
  { stroke : Maybe StrokeStyle 
  , fill : Maybe FillStyle }