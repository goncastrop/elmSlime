
import Keyboard
import Window

-- Inputs

type Input = { space:Bool, dirLX:Int, dirLY:Int, dirRX:Int, dirRY:Int, delta:Time }

delta = inSeconds <~ fps 50

input = sampleOn delta (Input <~ Keyboard.space
                               ~ lift .x Keyboard.wasd
                               ~ lift .y Keyboard.wasd
                               ~ lift .x Keyboard.arrows
                               ~ lift .y Keyboard.arrows
                               ~ delta)

-- Model

(gameWidth,gameHeight) = (700,400)
(halfWidth,halfHeight) = (gameWidth/2,gameHeight/2)

data State = Play | Pause | BetweenGames

netHeight = 40
netWidth = 5
ballInitPos = (halfWidth/2,0)

ballR = 10
playerRad = 35
eyeRad = 8

maxV = 600

bottom = playerRad-gameHeight/2

player x = { x=x, y=bottom, vx=0, vy=0, score=0 }
defaultGame =
  { state   = Pause,
    ball    = { x=0-halfWidth/2, y=0, vx=0, vy=0 },
    playerL = player (0-halfWidth/2) ,
    playerR = player (halfWidth/2) }

net = { x = 0, y = bottom + netHeight/2 }

-- Updates

near k c n = n >= k-c && n <= k+c

within ball p w h = (ball.x |> near p.x w)
                 && (ball.y |> near p.y h)

norma2 x y = sqrt((x)^2 + (y)^2)
within2 b p = norma2 (p.x-b.x) (p.y-b.y) < ballR + playerRad

calcAngle b p = atan2 (b.y-p.y) (b.x-p.x)
calcAngleNet b = 
  if | b.y <= bottom + netHeight && near 0 (netWidth/2+ballR) b.x && b.x < 0 -> pi
     | b.y <= bottom + netHeight && near 0 (netWidth/2+ballR) b.x && b.x > 0 -> 0
     | otherwise                                                             -> pi/2

-- funcion alternativa de paso de velocidad, fisica mas real pero tiene fallas
stepV b alfa = 
  let beta = calcAngle {x=(0-b.vx),y=(0-b.vy)} {x=0,y=0}
      gamma = if abs (alfa-beta) <= pi/2 then 2*alfa - beta else beta-pi
  in { b | vx <- maxV*(cos gamma), vy <- maxV*(sin gamma) }

stepV2 b alfa = { b | vx <- maxV*(cos alfa), vy <- maxV*(sin alfa) }

stepVWall b = if | b.x+ballR >= halfWidth   -> {b | vx <- 0 - abs b.vx }
              | b.x-ballR <= 0-halfWidth -> {b | vx <- abs b.vx }
              | otherwise                -> b

bounce b p1 p2 = 
  if | within2 b p1                                        -> stepV b (calcAngle b p1)
     | within2 b p2                                        -> stepV b (calcAngle b p2)
     | within b net (netWidth/2+ballR) (netHeight/2+ballR) -> stepV b (calcAngleNet b)
     | not (near 0 (halfWidth-ballR) b.x)                  -> stepVWall b
     | otherwise                                           -> b
     
gravity t m = if m.y > bottom then { m | vy <- m.vy - t*800 } else m
move2 t m = { m | x <- m.x + t*m.vx , y <- max bottom (m.y + t*m.vy) }
walk {x} m = { m | vx <- x*200}

jump {y} m = if y > 0 && m.y == bottom then { m | vy <- 350 } else m
updatePoints point m = { m | score <- m.score + point }

restrictPl p x1 x2 = { p | x <- clamp x1 x2 p.x }

stepBall2 t = move2 t . gravity t

stepPlyer t dir = move2 t . walk dir . gravity t . jump dir

stepBall t ball p1 p2 =
 if | ball.y <= bottom && ball.x < 0 -> { ball | x <- halfWidth/2, y <- 0 , vx <- 0, vy <- 0 }
    | ball.y <= bottom && ball.x > 0 -> { ball | x <- 0-halfWidth/2, y <- 50 , vx <- 0, vy <- 0 }
    | otherwise                      -> stepBall2 t (bounce ball p1 p2)

stepGame {space,dirLX,dirLY,dirRX,dirRY,delta} game =
  let {state,ball,playerL,playerR} = game
      scoreL = if ball.x > 0 && ball.y <= bottom then 1 else 0
      scoreR = if ball.x < 0 && ball.y <= bottom then 1 else 0
  in  {game| state   <- if | space                                                    -> Play
                           | scoreL+playerL.score == 10 || scoreR+playerR.score == 10 -> BetweenGames
                           | otherwise                                                -> state
           , ball    <- if state == Pause || state == BetweenGames then ball else
                            stepBall delta ball playerL playerR
           , playerL <- if | space && state == BetweenGames -> player (0-halfWidth/2)
                           | scoreL || scoreR -> updatePoints scoreL { playerL | x <- 0-halfWidth/2, y <- bottom}
                           | otherwise  ->  restrictPl (stepPlyer delta {x=dirLX,y=dirLY} playerL) (playerRad-halfWidth) (0-playerRad)
           , playerR <- if | space && state == BetweenGames -> player (halfWidth/2)
                           | scoreL || scoreR -> updatePoints scoreR { playerR | x <- halfWidth/2, y <- bottom}
                           | otherwise  ->  restrictPl (stepPlyer delta {x=dirRX,y=dirRY} playerR) (playerRad) (halfWidth-playerRad)}

gameState = foldp stepGame defaultGame input


-- Display

slimeGreen = rgb 60 100 60
textGreen = rgb 160 200 160
txt f = text . f . monospace . Text.color textGreen . toText
msg = "Slime water volleyball! SPACE to start, AWSD and &larr;&uarr;&darr;&rarr; to move"
msg2 = "Yellow player wins! Press space to start a new game"
msg3 = "Red player wins! Press space to start a new game"
make obj col shape = shape |> filled col
                           |> move (obj.x,obj.y)
eyeForm = group [ filled white (circle eyeRad), move (eyeRad/2,0) (filled black (circle (eyeRad/2))) ]

eye angle = rotate angle eyeForm

display (w,h) {state,ball,playerL,playerR} =
  let scores = txt (Text.height 4) (show playerL.score ++ "  " ++ show playerR.score)
      delt = (playerRad-eyeRad)*(cos (degrees 45))
      eyeL = {x=playerL.x+delt,y=playerL.y+delt}
      eyeR = {x=playerR.x-delt,y=playerR.y+delt}
  in container w h middle $ collage (round gameWidth) (round gameHeight)
       [ rect gameWidth gameHeight |> filled slimeGreen
       , rect netWidth netHeight |> make net white
       , circle ballR |> make ball white
       , circle playerRad |> make playerL yellow
       , circle playerRad |> make playerR red
       , eye (calcAngle ball eyeL) |> move (eyeL.x,eyeL.y)
       , eye (calcAngle ball eyeR) |> move (eyeR.x,eyeR.y)
       , rect gameWidth (gameHeight/2+bottom) |> make {x=0,y=playerRad/2-gameHeight/2} blue
       , toForm scores |> move (0, gameHeight/2 - 40)
       , toForm (if | state == Play -> spacer 1 1 
                    | state == BetweenGames && playerL.score == 10 -> txt id msg2
                    | state == BetweenGames && playerR.score == 10 -> txt id msg3
                    | otherwise -> txt id msg)
           |> move (0, bottom + 80)
       ]

main = lift2 display Window.dimensions gameState
