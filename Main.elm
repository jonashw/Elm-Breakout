import Window
import Keyboard

{-- Part 1: Model the user input ----------------------------------------------
What information do you need to represent all relevant user input?
------------------------------------------------------------------------------}

type UserInput = {space: Bool, paddle: Int}

userInput : Signal UserInput
userInput = lift2 UserInput Keyboard.space (lift .x Keyboard.arrows)

type Input = { timeDelta:Float, userInput:UserInput }

{-- Part 2: Model the game ----------------------------------------------------
What information do you need to represent the entire game?
------------------------------------------------------------------------------}
(gameWidth,gameHeight) = (500,900)
(halfWidth,halfHeight) = (gameWidth / 2, gameHeight / 2)

type Positioned a = { a | x : Float, y : Float }
type Moving a = { a | vx : Float, vy : Float }
type Sized a = { a | width : Float, height : Float }
type Vector = {x : Float, y : Float}
data PlayState = Play | Pause
type Paddle = { x : Float, y : Float, size : Vector }
type Ball = {x : Float, y : Float, vx : Float, vy : Float}
type GameState = {playState : PlayState, paddle : Paddle, balls: [Ball]}
ballSize = 10.0

defaultGame : GameState
defaultGame = { playState = Pause
              , paddle = { x = 0, y = 10 - halfHeight, size = { x = 200, y = 20 } }
              , balls = [ { vx = 0.5, vy = 0.5, x = 0, y = 0 } ]
              }

{-- Part 3: Update the game ---------------------------------------------------
How does the game step from one state to another based on user input?
------------------------------------------------------------------------------}
stepPlayState : Bool -> PlayState -> PlayState
stepPlayState buttonPressed playState = if(buttonPressed)
                                        then (if(playState == Play) then Pause else Play)
                                        else playState

maxObjX : Float -> Float
maxObjX objWidth = (gameWidth - objWidth) /2

minObjX : Float -> Float
minObjX objWidth = (objWidth - gameWidth) /2

maxObjY : Float -> Float
maxObjY objHeight = (gameHeight - objHeight) /2

minObjY : Float -> Float
minObjY objHeight = (objHeight - gameHeight) /2

stepBall : Time -> Ball -> Ball 
stepBall timeDelta ball = let min = { x = minObjX ballSize, y = minObjY ballSize }
                              max = { x = maxObjX ballSize, y = maxObjY ballSize } 
                              x' = clamp min.x max.x (ball.x + ball.vx * timeDelta)
                              y' = clamp min.y max.y (ball.y + ball.vy * timeDelta)
                              vx' = if (x' == min.x || x' == max.x) then -ball.vx else ball.vx
                              vy' = if (y' == min.y || y' == max.y) then -ball.vy else ball.vy
                          in { ball | x <- x', y <- y', vx <- vx', vy <- vy' }

stepPaddle : Time -> Int -> Paddle -> Paddle
stepPaddle timeDelta dir paddle = let minX = minObjX paddle.size.x
                                      maxX = maxObjX paddle.size.x
                                      vx'       = toFloat dir
                                      x'        = paddle.x + (vx' * timeDelta)
                                  in { paddle | x <- clamp minX maxX x' }

stepGame : Input -> GameState -> GameState
stepGame {timeDelta,userInput} gameState = { gameState 
                                           | playState <- stepPlayState userInput.space gameState.playState
                                           , paddle <- stepPaddle timeDelta userInput.paddle gameState.paddle
                                           , balls <- map (stepBall timeDelta) gameState.balls
                                           }

{-- Part 4: Display the game --------------------------------------------------
How should the GameState be displayed to the user?
------------------------------------------------------------------------------}

displayArena = rect gameWidth gameHeight |> outlined defaultLine

displayPaddle : Paddle -> Form
displayPaddle paddle = rect 200 20 |> filled red
                                   |> move (paddle.x, paddle.y)

displayBall : Ball -> Form
displayBall ball = circle ballSize |> filled blue
                                   |> move (ball.x, ball.y)

display : (Int,Int) -> GameState -> Element
display (w,h) gameState = flow down 
                          [ asText gameState
                          , collage gameWidth gameHeight
                              ([ displayArena
                              , displayPaddle gameState.paddle
                              ] ++ (map displayBall gameState.balls))
                          ]

{-- That's all folks! ---------------------------------------------------------
The following code puts it all together and shows it on screen.
------------------------------------------------------------------------------}

delta = fps 30
input = sampleOn delta (lift2 Input delta userInput)

gameState = foldp stepGame defaultGame input

main = lift2 display Window.dimensions gameState



{-- lib --}

cartProd : [a] -> [b] -> [(a,b)]
cartProd xs ys = if ((length xs) == 0 || (length ys) == 0)
                then []
                else map (\y -> (head xs, y)) ys ++ cartProd (tail xs) ys

range : Int -> [Int]
range n = if (n < 0)
          then [0]
          else n :: range (n-1)

