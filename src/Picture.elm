module Picture exposing (..)

import Box exposing (..)
import Lens exposing (..)
import Shape exposing (..)
import Style exposing (..)

type alias Rendering = List (Shape, Style)

type alias Picture = Lens -> Rendering

blank : Picture 
blank _ = []

turn : Picture -> Picture
turn p = turnLens >> p

times : Int -> (a -> a) -> (a -> a)
times n fn = identity

turns : Int -> (Picture -> Picture)
turns n = times n turn 

flip : Picture -> Picture 
flip p = flipLens >> p 

toss : Picture -> Picture 
toss p = tossLens >> p 

aboveRatio : Int -> Int -> Picture -> Picture -> Picture 
aboveRatio m n p1 p2 =
  \lens -> 
    let 
      f = toFloat m / toFloat (m + n)
      (l1, l2) = splitLensVertically f lens 
    in 
      (p1 l1) ++ (p2 l2)

above : Picture -> Picture -> Picture 
above = aboveRatio 1 1  

besideRatio : Int -> Int -> Picture -> Picture -> Picture
besideRatio m n p1 p2 =
  \lens ->
    let
      f = toFloat m / toFloat (m + n)
      (l1, l2) = splitLensHorizontally f lens
    in
      (p1 l1) ++ (p2 l2)

beside : Picture -> Picture -> Picture 
beside = besideRatio 1 1 

quartet : Picture -> Picture -> Picture -> Picture -> Picture
quartet nw ne sw se =
  above (beside nw ne)
        (beside sw se)

nonet : Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
nonet nw nm ne mw mm me sw sm se =
  let
    row w m e = besideRatio 1 2 w (beside m e)
    col n m s = aboveRatio 1 2 n (above m s)
  in
    col (row nw nm ne)
        (row mw mm me)
        (row sw sm se)

over : Picture -> Picture -> Picture
over p1 p2 = 
  \box -> (p1 box) ++ (p2 box)

ttile : Picture -> Picture
ttile fish =
  let
    fishN = fish |> toss |> flip
    fishE = fishN |> turn |> turn |> turn
  in
    fish |> over fishN |> over fishE

utile : Picture -> Picture 
utile fish =
  let 
    fishN = fish |> toss |> flip 
    fishW = fishN |> turn
    fishS = fishW |> turn 
    fishE = fishS |> turn
  in  
    fishN |> over fishW |> over fishS |> over fishE  

side : Int -> Picture -> Picture 
side n fish = 
  if n < 1 then blank
  else 
    let 
      s = side (n - 1) fish 
      t = ttile fish 
    in 
      quartet s s (turn t) t

corner : Int -> Picture -> Picture
corner n fish =
  if n < 1 then blank
  else
    let
      c = corner (n - 1) fish
      s = side (n - 1) fish
    in
      quartet c s (turn s) (utile fish)

squareLimit : Int -> Picture -> Picture
squareLimit n fish =
  let
    mm = utile fish
    nw = corner n fish
    sw = nw |> turn
    se = sw |> turn
    ne = se |> turn
    nm = side n fish
    mw = nm |> turn
    sm = mw |> turn
    me = sm |> turn
  in
    nonet nw nm ne mw mm me sw sm se
