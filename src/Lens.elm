module Lens exposing (..)

import Box exposing (..)

type Hue = Blackish | Greyish | Whiteish

type alias Lens = (Box, Hue)

turnLens : Lens -> Lens
turnLens (box, hue) = (turnBox box, hue)

flipLens : Lens -> Lens
flipLens (box, hue) = (flipBox box, hue)
 
tossLens : Lens -> Lens
tossLens (box, hue) = (tossBox box, hue)

scaleLensHorizontally : Float -> Lens -> Lens
scaleLensHorizontally s (box, hue) = 
  (scaleHorizontally s box, hue)

scaleLensVertically : Float -> Lens -> Lens
scaleLensVertically s (box, hue) =  
  (scaleVertically s box, hue)

moveLensHorizontally : Float -> Lens -> Lens
moveLensHorizontally offset (box, hue) = 
  (moveHorizontally offset box, hue)
  
moveLensVertically : Float -> Lens -> Lens
moveLensVertically offset (box, hue) = 
  (moveVertically offset box, hue)

splitLensHorizontally : Float -> Lens -> (Lens, Lens)
splitLensHorizontally f (box, hue) =
  let (box1, box2) = splitHorizontally f box
  in ((box1, hue), (box2, hue))

splitLensVertically : Float -> Lens -> (Lens, Lens)
splitLensVertically f (box, hue) = 
  let (box1, box2) = splitVertically f box
  in ((box1, hue), (box2, hue))

rehueLens : Lens -> Lens
rehueLens (box, hue) = 
  let 
    change h = 
      case h of 
      Whiteish -> Blackish
      Greyish -> Whiteish
      Blackish -> Greyish
  in 
    (box, change hue)