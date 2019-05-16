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
times n fn =
  if n < 1 then identity
  else fn >> times (n - 1) fn
  
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

row : List Picture -> Picture 
row ps = 
  case ps of 
    [] -> blank
    h :: t -> besideRatio 1 (List.length ps - 1) h (row t)

col : List Picture -> Picture   
col ps = 
  case ps of 
    [] -> blank
    h :: t -> aboveRatio 1 (List.length ps - 1) h (col t)

nonet : Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
nonet nw nm ne mw mm me sw sm se =
  let
    rw w m e = besideRatio 1 2 w (beside m e)
    cl n m s = aboveRatio 1 2 n (above m s)
  in
    cl (rw nw nm ne)
       (rw mw mm me)
       (rw sw sm se)

over : Picture -> Picture -> Picture
over p1 p2 = 
  \box -> (p1 box) ++ (p2 box)

rehue : Picture -> Picture 
rehue p =
  rehueLens >> p

ttile : (Picture -> Picture) -> (Picture -> Picture) -> Picture -> Picture 
ttile rehueN rehueE fish =
  let 
    fishN = fish |> toss |> flip
    fishE = fishN |> turn |> turn |> turn
  in 
    over fish (over (rehueN fishN) (rehueE fishE))

ttile1 : Picture -> Picture 
ttile1 = ttile rehue (rehue >> rehue)

ttile2 : Picture -> Picture
ttile2 = ttile (rehue >> rehue) rehue

ttile0 : Picture -> Picture
ttile0 fish =
  let
    fishN = fish |> toss |> flip
    fishE = fishN |> turn |> turn |> turn
  in
    fish |> over fishN |> over fishE

utile0 : Picture -> Picture 
utile0 fish =
  let 
    fishN = fish |> toss |> flip 
    fishW = fishN |> turn
    fishS = fishW |> turn 
    fishE = fishS |> turn
  in  
    fishN |> over fishW |> over fishS |> over fishE  

utile : (Picture -> Picture) -> (Picture -> Picture) -> (Picture -> Picture) -> (Picture -> Picture) -> Picture -> Picture
utile rehueN rehueW rehueS rehueE fish =
  let 
    fishN = fish |> toss |> flip
    fishW = turn fishN
    fishS = turn fishW
    fishE = turn fishS
  in over (over (rehueN fishN) (rehueW fishW))
          (over (rehueS fishS) (rehueE fishE))

utile1 : Picture -> Picture
utile1 = utile (rehue >> rehue) identity (rehue >> rehue) identity

utile2 : Picture -> Picture
utile2 = utile identity (rehue >> rehue) rehue (rehue >> rehue)

utile3 : Picture -> Picture
utile3 = utile (rehue >> rehue) identity rehue identity

side0 : Int -> Picture -> Picture 
side0 n fish = 
  if n < 1 then blank
  else 
    let 
      s = side0 (n - 1) fish 
      t = ttile0 fish 
    in 
      quartet s s (turn t) t

side : (Picture -> Picture) -> (Picture -> Picture) -> (Picture -> Picture) -> Int -> Picture -> Picture
side tt rehueSW rehueSE n fish =
  let 
    t = tt fish
    recur c =
      let r = if c == 1 then blank else recur (c - 1)
      in quartet r r (t |> turn |> rehueSW) (t |> rehueSE)
  in 
    recur n

sideNS : Int -> Picture -> Picture 
sideNS = side ttile1 identity rehue

sideEW : Int -> Picture -> Picture 
sideEW = side ttile2 (rehue >> rehue) rehue

corner0 : Int -> Picture -> Picture
corner0 n fish =
  if n < 1 then blank
  else
    let
      c = corner0 (n - 1) fish
      s = side0 (n - 1) fish
    in
      quartet c s (turn s) (utile0 fish)

corner : (Picture -> Picture) -> (Int -> Picture -> Picture) -> (Int -> Picture -> Picture) -> Int -> Picture -> Picture
corner ut side1 side2 n fish =
  let 
    u = ut fish
    fn x =
      let 
        (c, ne, sw) =
          if x == 1 then (blank, blank, blank)
                    else (fn (x - 1), side1 (x - 1) fish, side2 (x - 1) fish)
      in 
        quartet c ne (sw |> turn) u
  in 
    fn n

cornerNWSE : Int -> Picture -> Picture
cornerNWSE = corner utile3 sideNS sideEW

cornerNESW : Int -> Picture -> Picture
cornerNESW = corner utile2 sideEW sideNS

squareLimit0 : Int -> Picture -> Picture
squareLimit0 n fish =
  let
    mm = utile0 fish
    nw = corner0 n fish
    sw = nw |> turn
    se = sw |> turn
    ne = se |> turn
    nm = side0 n fish
    mw = nm |> turn
    sm = mw |> turn
    me = sm |> turn
  in
    nonet nw nm ne mw mm me sw sm se

squareLimit : Int -> Picture -> Picture
squareLimit n fish =
  let 
    cornerNW = cornerNWSE n fish
    cornerSW = cornerNESW n fish |> turn
    cornerSE = cornerNW |> turn |> turn
    cornerNE = cornerSW |> turn |> turn
    sideN = sideNS n fish
    sideW = sideEW n fish |> turn
    sideS = sideN |> turn |> turn
    sideE = sideW |> turn |> turn
    center = utile1 fish
  in 
    nonet cornerNW sideN cornerNE
          sideW center sideE
          cornerSW sideS cornerSE
