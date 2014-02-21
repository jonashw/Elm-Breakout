import open Lib
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

type Vector = {x : Float, y : Float}
data PlayState = Play | Pause
type Paddle = { position : Vector, size : Vector }
type Ball = {velocity: Vector, position: Vector}
type GameState = {playState : PlayState, paddle : Paddle, balls: [Ball]}
ballSize = 10.0

defaultGame : GameState
defaultGame = { playState = Pause
              , paddle = { position = {x = 0, y = 10 - halfHeight}, size = { x = 200, y = 20 } }
              , balls = [ { velocity = {x = 0.5, y = 0.5}, position = {x = 0, y = 0} } ]
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
                              x' = ball.position.x + ball.velocity.x * timeDelta
                              y' = ball.position.y + ball.velocity.y * timeDelta 
                              position' = { x = clamp min.x max.x x', y = clamp min.y max.y y' }
                              vx' = if (position'.x == min.x || position'.x == max.x) then -ball.velocity.x else ball.velocity.x
                              vy' = if (position'.y == min.y || position'.y == max.y) then -ball.velocity.y else ball.velocity.y
                              velocity' = { x = vx', y = vy' }
                          in { ball | position <- position', velocity <- velocity' }

stepPaddle : Time -> Int -> Paddle -> Paddle
stepPaddle timeDelta dir paddle = let position' = paddle.position
                                      minX = minObjX paddle.size.x
                                      maxX = maxObjX paddle.size.x
                                      vx'       = toFloat dir
                                      x'        = position'.x + (vx' * timeDelta)
                                      pos'      = { position' | x <- clamp minX maxX x' }
                                  in { paddle | position <- pos' }

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
                                   |> move (paddle.position.x, paddle.position.y)

displayBall : Ball -> Form
displayBall ball = circle ballSize |> filled blue
                                   |> move (ball.position.x, ball.position.y)

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
