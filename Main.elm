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
type Paddle = { position : Vector, width : Int, height : Int}
type Ball = {velocity: Vector, position: Vector}
type GameState = {playState : PlayState, paddle : Paddle, balls: [Ball]}
ballSize = 10.0

defaultGame : GameState
defaultGame = { playState = Pause
              , paddle = { position = {x = 0, y = 10 - halfHeight}, width = 200, height = 20}
              , balls = [ { velocity = {x = 0.5, y = 0.5}, position = {x = 0, y = 0} } ]
              }


{-- Part 3: Update the game ---------------------------------------------------
How does the game step from one state to another based on user input?
------------------------------------------------------------------------------}
stepPlayState : Bool -> PlayState -> PlayState
stepPlayState buttonPressed playState = if(buttonPressed)
                                        then (if(playState == Play) then Pause else Play)
                                        else playState

clampPaddleX : Paddle -> Float
clampPaddleX paddle = let minX = ((toFloat paddle.width - gameWidth) / 2)
                          maxX = ((gameWidth - (toFloat paddle.width)) / 2)
                          pos = paddle.position
                      in clamp minX maxX pos.x

clampBallX : Ball -> Ball
clampBallX ball = let minX = (ballSize - gameWidth) / 2
                      maxX = (gameWidth - ballSize) / 2
                      x'   = clamp minX maxX ball.position.x
                      vx'  = if (x' == minX || x' == maxX)
                             then -ball.velocity.x
                             else ball.velocity.x
                      velocity =  ball.velocity
                      velocity' = { velocity | x <- vx' }
                      position = ball.position
                      position' = { position | x <- x' }
                  in { ball | position <- position', velocity <- velocity' }

clampBallY : Ball -> Ball
clampBallY ball = let minY = (ballSize - gameHeight) / 2
                      maxY = (gameHeight - ballSize) / 2
                      y'   = clamp minY maxY ball.position.y
                      vy'  = if (y' == minY || y' == maxY)
                             then -ball.velocity.y
                             else ball.velocity.y
                      velocity =  ball.velocity
                      velocity' = { velocity | y <- vy' }
                      position = ball.position
                      position' = { position | y <- y' }
                  in { ball | position <- position', velocity <- velocity' }

stepBall : Time -> Ball -> Ball 
stepBall timeDelta ball = let position = ball.position
                              position' = { position | x <- ball.position.x + ball.velocity.x * timeDelta
                                                     , y <- ball.position.y + ball.velocity.y * timeDelta }
                              ball' = { ball | position <- position' }
                          in clampBallY (clampBallX ball')
 
stepPaddle : Time -> Int -> Paddle -> Paddle
stepPaddle timeDelta dir paddle = let vx'     = toFloat dir
                                      pos'    = paddle.position
                                      position' = { pos' | x <- paddle.position.x + (vx' * timeDelta) } 
                                      paddle' = { paddle | position <- position' }
                                      position'' = { position' | x <- clampPaddleX paddle' }
                                  in { paddle' | position <- position'' }

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
