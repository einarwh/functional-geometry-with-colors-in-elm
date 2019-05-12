module Fitting exposing (createPicture)

import Vector exposing (Vector, add, scale, length) 
import Box exposing (Box)
import Lens exposing (..)
import Shape exposing (..)
import Style exposing (..)
import Picture exposing (Picture)

mapper : Box -> Vector -> Vector 
mapper { a, b, c } { x, y } =
  add a (add (scale x b) (scale y c))

getStrokeWidth : Box -> Float
getStrokeWidth { b, c } =
  let 
    s = max (length b) (length c) 
  in
    s / 80.0

getStyle : Box -> Style
getStyle box = 
  let sw = getStrokeWidth box in
  { stroke = Just { strokeWidth = sw
                  , strokeColor = B } 
  , fill = Nothing }

mapShape : (Vector -> Vector) -> Shape -> Shape 
mapShape m shape = 
  case shape of  
    Polygon { points } -> Polygon { points = List.map m points }
   
    Polyline { pts } -> Polyline { pts = List.map m pts }

    Curve { point1, point2, point3, point4 } ->
      Curve { point1 = m point1 
            , point2 = m point2 
            , point3 = m point3 
            , point4 = m point4 } 

    Path (startVector, beziers) -> 
      Path (m startVector, List.map (mapBezier m) beziers)

    x -> x

mapBezier : (Vector -> Vector) -> BezierShape -> BezierShape 
mapBezier m bz =
  { controlPoint1 = m bz.controlPoint1
  , controlPoint2 = m bz.controlPoint2
  , endPoint = m bz.endPoint }

getColor : Name -> Hue -> StyleColor
getColor name hue = 
  case hue of  
    Blackish -> 
      if name == "primary" then B  
      else if isOuterEye name then W
      else if isInnerEye name then B 
      else W
    Greyish -> 
      if name == "primary" then G
      else if isOuterEye name then W 
      else if isInnerEye name then G 
      else W 
    Whiteish -> 
      if name == "primary" then W 
      else if isOuterEye name then W  
      else if isInnerEye name then B 
      else B

isInnerEye : Name -> Bool
isInnerEye name = 
  name == "eye-inner" || name == "egg-eye-inner"

isOuterEye : Name -> Bool
isOuterEye name = 
  name == "eye-outer" || name == "egg-eye-outer"

getEyeLiner : Float -> Hue -> StrokeStyle
getEyeLiner sw hue =  
  { strokeColor = getColor "secondary" hue 
  , strokeWidth = sw }

getPathStyle : Name -> Float -> Hue -> Style
getPathStyle name sw hue = 
  let 
    stroke = if isOuterEye name then Just (getEyeLiner sw hue) else Nothing
    fill = Just { fillColor = getColor name hue }
  in 
    { stroke = stroke, fill = fill }

getDefaultColor : Name -> Hue -> StyleColor
getDefaultColor name hue = 
  if name == "secondary" then 
    case hue of 
      Blackish -> W
      Greyish -> W
      Whiteish -> B
  else
    case hue of 
      Blackish -> B
      Greyish -> G
      Whiteish -> W

getDefaultStyle : Name -> Hue -> Float -> Style
getDefaultStyle name hue sw = 
  let 
    stroke = 
      { strokeWidth = sw 
      , strokeColor = getDefaultColor name hue }
  in 
    { stroke = Just stroke, fill = Nothing }

mapNamedShape : Lens -> NamedShape -> (Shape, Style)
mapNamedShape (box, hue) (name, shape) = 
  let 
    m = mapper box
    sw = getStrokeWidth box
  in 
    case shape of
    Polygon { points } ->
      (Polygon { points = List.map m points }, getDefaultStyle name hue sw)
    Curve { point1, point2, point3, point4 } ->
      (Curve { point1 = m point1
             , point2 = m point2 
             , point3 = m point3 
             , point4 = m point4 }, getDefaultStyle name hue sw)
    Path (start, beziers) ->
      let style = getPathStyle name sw hue
      in (Path (m start, beziers |> List.map (mapBezier m)), style)
    Line { lineStart, lineEnd } ->
      (Line { lineStart = m lineStart 
            , lineEnd = m lineEnd }, getDefaultStyle name hue sw)
    _ ->
      let nv = { x = 0, y = 0 } 
      in 
        (Line { lineStart = nv, lineEnd = nv }, getDefaultStyle name hue sw)

createPicture : List NamedShape -> Picture
createPicture shapes lens = 
  let 
    (box, hue) = lens
    m = mapper box
    style = getStyle box 
  in 
    shapes |> List.map (mapNamedShape lens)
