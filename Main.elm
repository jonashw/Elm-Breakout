import open Lib
import Window
import Keyboard

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type UserInput = {space: Bool, paddle: Int}

userInput : Signal UserInput
userInput = lift2 UserInput Keyboard.space (lift .x Keyboard.arrows)

type Input = { timeDelta:Float, userInput:UserInput }



{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

Tasks: Redefine `GameState` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.

For example, if you want to represent many objects that just have a position,
your GameState might just be a list of coordinates and your default game might
be an empty list (no objects at the start):

    type GameState = { objects : [(Float,Float)] }
    defaultGame = { objects = [] }

------------------------------------------------------------------------------}
(gameWidth,gameHeight) = (500,900)
(halfWidth,halfHeight) = (gameWidth / 2, gameHeight / 2)

data PlayState = Play | Pause
type Paddle = {vx : Float, vy : Float, x : Float, y : Float, width : Int, height : Int}
type Ball = {vx : Float, vy : Float, x : Float, y : Float}
type GameState = {playState : PlayState, paddle : Paddle, balls: [Ball]}
ballSize = 10.0

defaultGame : GameState
defaultGame = { playState = Pause
              , paddle = {vx = 0, vy = 0, x = 0, y = 10 - halfHeight, width = 200, height = 20}
              , balls = [{vx = 0.5, vy = 0.5, x = 0, y = 0}]
              }


{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}
stepPlayState : Bool -> PlayState -> PlayState
stepPlayState buttonPressed playState = if(buttonPressed)
                                        then (if(playState == Play) then Pause else Play)
                                        else playState

clampPaddleX : Paddle -> Float
clampPaddleX paddle = let minX = ((toFloat paddle.width - gameWidth) / 2)
                          maxX = ((gameWidth - (toFloat paddle.width)) / 2)
                      in clamp minX maxX paddle.x

clampBallX : Ball -> Ball
clampBallX ball = let minX = (ballSize - gameWidth) / 2
                      maxX = (gameWidth - ballSize) / 2
                      x'   = clamp minX maxX ball.x
                      vx'  = if (x' == minX || x' == maxX)
                             then -ball.vx
                             else ball.vx
                  in { ball | x <- x', vx <- vx' }

clampBallY : Ball -> Ball
clampBallY ball = let minY = (ballSize - gameHeight) / 2
                      maxY = (gameHeight - ballSize) / 2
                      y'   = clamp minY maxY ball.y
                      vy'  = if (y' == minY || y' == maxY)
                             then -ball.vy
                             else ball.vy
                  in { ball | y <- y', vy <- vy' }

stepBall : Time -> Ball -> Ball 
stepBall timeDelta ball = let ball' = { ball
                                      | x <- ball.x + ball.vx * timeDelta
                                      , y <- ball.y + ball.vy * timeDelta
                                      }
                          in clampBallY (clampBallX ball')
 
stepPaddle : Time -> Int -> Paddle -> Paddle
stepPaddle timeDelta dir paddle = let vx'     = toFloat dir
                                      x'      = paddle.x + (vx' * timeDelta)
                                      paddle' = { paddle | x <- x', vx <- vx'}
                                  in {paddle' | x <- clampPaddleX paddle'}

stepGame : Input -> GameState -> GameState
stepGame {timeDelta,userInput} gameState = { gameState 
                                           | playState <- stepPlayState userInput.space gameState.playState
                                           , paddle <- stepPaddle timeDelta userInput.paddle gameState.paddle
                                           , balls <- map (stepBall timeDelta) gameState.balls
                                           }


{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

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
