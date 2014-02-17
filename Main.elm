import open Lib

main = collage arenaWidth arenaHeight <| center :: arenaOutline :: originMarker :: brickForms



-- Setup
(arenaWidth, arenaHeight) = (500, 800)
(arenaHalfWidth, arenaHalfHeight) = (arenaWidth / 2, arenaHeight / 2)

(brickCols, brickRows) = (20,80)
(brickWidth, brickHeight) = (arenaWidth / brickCols, arenaHeight / brickRows)
          
brickCoords = let rawPairs = cartProd [-9..9] [35..39]
                  converter (x,y) = (x * brickWidth, y * brickHeight)
              in map converter rawPairs

drawBrick : (number,number) -> Form
drawBrick coord = rect brickWidth brickHeight
                  |> outlined defaultLine
                  |> move coord

brickForms = map drawBrick brickCoords

originMarker = square 4
               |> filled blue
               |> move (-298,0)

arenaOutline = rect arenaWidth arenaHeight
               |> outlined defaultLine





-- center stuff
centerSquare = square 3 |> outlined defaultLine
centerDots = let lineLength = 6
             in group 
               [ centerLine lineLength 1 ( lineLength,0)
               , centerLine lineLength 1 (-lineLength,0)
               , centerLine 1 lineLength (0, lineLength)
               , centerLine 1 lineLength (0,-lineLength)
               ]

centerLine width height coord = rect width height
                                |> filled black
                                |> move coord

center = group [centerSquare, centerDots]
