import Keyboard
import Window
import Text
import Random


(gameWidth, gameHeight) = (600, 400)
(halfWidth, halfHeight) = (300, 200)
(fourthWidth, fourthHeight) = (150, 100)
velocity = 50

data Direction = Left | Right | Up | Down
data Frogstate = Alive | Dead | Occupied | Unstarted
data State = Play | Stop

type Input = { dir:{x:Int, y:Int} , delta:Time }

delta = inSeconds <~ fps 10

input = sampleOn delta (Input <~ Keyboard.arrows
                               ~ delta)
--input = Keyboard.arrows


type Frog = { state:Frogstate, direction:Direction, x:Float, y:Float }
type Car = { x:Float, y:Float }
type House = { x:Float, y:Float, state:Bool }
type Game = { state:State, house:House, house1:House, house2:House, house3:House, house4:House, frog:Frog, frog1:Frog, frog2:Frog, frog3:Frog, frog4:Frog, car:Car, car1:Car, car2:Car, car3:Car, card:Car, card1:Car, card2:Car, card3:Car }

defaultGame : Game
defaultGame =
  { state = Play,
    house = { x = halfWidth*2/3, y = fourthHeight*1.5, state = False }, 
    house1 = { x = halfWidth/3, y = fourthHeight*1.5, state = False },
    house2 = { x = 0, y = fourthHeight*1.5, state = False },
    house3 = { x = -halfWidth/3, y = fourthHeight*1.5, state = False },
    house4 = { x = -halfWidth*2/3, y = fourthHeight*1.5, state = False },
    frog = { state = Alive, direction = Up, x = 0, y = -fourthHeight*1.5 },
    frog1 = { state = Unstarted, direction = Up, x = 0, y = -fourthHeight*3 },
    frog2 = { state = Unstarted, direction = Up, x = 0, y = -fourthHeight*3 },
    frog3 = { state = Unstarted, direction = Up, x = 0, y = -fourthHeight*3 },
    frog4 = { state = Unstarted, direction = Up, x = 0, y = -fourthHeight*3 },
    car = { x = halfWidth*1.05 , y = fourthHeight/2 },
    car1 = { x = fourthWidth*0.93, y = fourthHeight/2 },
    car2 = { x = 0, y = fourthHeight/2 },
    car3 = { x = -fourthWidth*1.1, y = fourthHeight/2 },
    card = { x = -halfWidth*1.03, y = -fourthHeight/2 },
    card1 = { x = -fourthWidth*0.8, y = -fourthHeight/2 },
    card2 = { x = 0, y = -fourthHeight/2 },
    card3 = { x = fourthWidth*1.12, y = -fourthHeight/2 }
  }

jumpUp frog =
  { frog | y <- frog.y + fourthHeight }

jumpDown frog =
  { frog | y <- frog.y - fourthHeight }
  
jumpLeft frog =
  { frog | x <- frog.x - fourthHeight }  

jumpRight frog =
  { frog | x <- frog.x + fourthHeight }

jump frog = if |frog.direction == Left -> jumpLeft frog
               |frog.direction == Right -> jumpRight frog
               |frog.direction == Up -> jumpUp frog
               |otherwise -> jumpDown frog                       


rotateRight frog =
  { frog | direction <- if |frog.direction == Left -> Down
                           |frog.direction == Right -> Up
                           |frog.direction == Up -> Left
                           |otherwise -> Right
  }
  
rotateLeft frog =
  { frog | direction <- if |frog.direction == Left -> Up
                           |frog.direction == Right -> Down
                           |frog.direction == Up -> Right
                           |otherwise -> Left
  }

near k c n = n >= k-c && n <= k+c
within frog car = (frog.x |> near car.x 20) && (frog.y |> near car.y 15)
checkwithin frog cars = case cars of
                          [] -> False
                          hd :: tl -> if |(within frog hd) -> True
                                         |otherwise -> checkwithin frog tl
                                         
changehousestate house = 
  { house | state <- True }

checkhouse frog houses = case houses of
                          [] -> False
                          hd :: tl -> if |hd.state == False && (within frog hd) -> True
                                         |otherwise -> checkhouse frog tl
frogdead frog = 
  { frog | state <- Dead }
  
frogoccupied frog =
  { frog | state <- Occupied }
  
frogalive frog = 
  { frog | y <- -fourthHeight*1.5,
           state <- Alive }

motionFrog dir frog = if |dir.y == 1 -> jump frog
                         |dir.x == 1 -> rotateLeft frog
                         |dir.x == -1 -> rotateRight frog
                         |otherwise -> frog
                           
stepFrog dir frog houses cars = if |frog.state == Dead -> frog
                                   |frog.state == Occupied -> frog
                                   |(checkwithin frog cars) -> frogdead frog
                                   |(checkhouse frog houses) -> frogoccupied frog
                                   |otherwise -> motionFrog dir frog
                              
stepFollow dir currentfrog prevfrog houses cars = if |prevfrog.state == Alive -> currentfrog
                                                     |prevfrog.state == Unstarted -> currentfrog
                                                     |currentfrog.state == Dead -> currentfrog
                                                     |currentfrog.state == Occupied -> currentfrog
                                                     |(checkwithin currentfrog cars) -> frogdead currentfrog
                                                     |(checkhouse currentfrog houses) -> frogoccupied currentfrog
                                                     |currentfrog.state == Alive -> motionFrog dir currentfrog
                                                     |prevfrog.state == Occupied -> frogalive currentfrog
                                                     |prevfrog.state == Dead -> frogalive currentfrog
                                                     |otherwise -> motionFrog dir currentfrog

stepCarUp delta car = 
  { car | x <- if |car.x <= -halfWidth -> halfWidth 
                  |otherwise -> car.x - velocity * delta
  }
  
stepCarDown delta card =
  { card | x <- if |card.x >= halfWidth -> -halfWidth
                   |otherwise -> card.x + velocity * delta
  }

stepEachFrog frog house =
  { house | state <- if |house.state == True -> True
                        |(within frog house) == True -> True
                        |otherwise -> False }

stepHouse frogs house = 
  case frogs of
    [] -> house
    hd::tl -> stepHouse tl (stepEachFrog hd house)

stepState finalfrog state = if |finalfrog.state == Occupied -> Stop
                               |finalfrog.state == Dead -> Stop
                               |otherwise -> Play

stepGame {dir, delta} ( {state, house, house1, house2, house3, house4, frog, frog1, frog2, frog3, frog4, car, car1, car2, car3, card, card1, card2, card3} as game) = 
    let houses = [house, house1, house2, house3, house4]
        cars = [car, car1, car2, car3, card, card1, card2, card3]
        frogs = [frog, frog1, frog2, frog3, frog4]
    in {game | state <- stepState frog4 state
          , house <- stepHouse frogs house
          , house1 <- stepHouse frogs house1
          , house2 <- stepHouse frogs house2
          , house3 <- stepHouse frogs house3
          , house4 <- stepHouse frogs house4 
          , frog <- stepFrog dir frog houses cars
          , frog1 <- stepFollow dir frog1 frog houses cars
          , frog2 <- stepFollow dir frog2 frog1 houses cars
          , frog3 <- stepFollow dir frog3 frog2 houses cars
          , frog4 <- stepFollow dir frog4 frog3 houses cars
          , car <- stepCarUp delta car
          , car1 <- stepCarUp delta car1
          , car2 <- stepCarUp delta car2
          , car3 <- stepCarUp delta car3
          , card <- stepCarDown delta card
          , card1 <- stepCarDown delta card1
          , card2 <- stepCarDown delta card2
          , card3 <- stepCarDown delta card3
    }


gameState = foldp stepGame defaultGame input

make obj shape = 
    shape |> move (obj.x, obj.y)
          
makefrog frog shape =
    shape |> move (frog.x, frog.y)
 
txt f = toText >> Text.color black >> monospace >> f >> leftAligned          

display (w, h) {state, house, house1, house2, house3, house4, frog, frog1, frog2, frog3, frog4, car, car1, car2, car3, card, card1, card2, card3} = 
    if |state == Stop -> container w h middle <| collage gameWidth gameHeight
                            [ rect gameWidth gameHeight |> filled grey
                            , toForm (txt (Text.height 50) "Game Over") ]
       |otherwise ->
              let src = if |frog.state == Dead -> "splat.gif"
                           |frog.direction == Left -> "frog-left.png"
                           |frog.direction == Right -> "frog-right.png"
                           |frog.direction == Up -> "frog-up.png"
                           |otherwise -> "frog-down.png"
                  src1 = if |frog1.state == Dead -> "splat.gif"
                            |frog1.direction == Left -> "frog-left.png"
                            |frog1.direction == Right -> "frog-right.png"
                            |frog1.direction == Up -> "frog-up.png"
                            |otherwise -> "frog-down.png"
                  src2 = if |frog2.state == Dead -> "splat.gif"
                            |frog2.direction == Left -> "frog-left.png"
                            |frog2.direction == Right -> "frog-right.png"
                            |frog2.direction == Up -> "frog-up.png"
                            |otherwise -> "frog-down.png"
                  src3 = if |frog3.state == Dead -> "splat.gif"
                            |frog3.direction == Left -> "frog-left.png"
                            |frog3.direction == Right -> "frog-right.png"
                            |frog3.direction == Up -> "frog-up.png"
                            |otherwise -> "frog-down.png"
                  src4 = if |frog4.state == Dead -> "splat.gif"
                            |frog4.direction == Left -> "frog-left.png"
                            |frog4.direction == Right -> "frog-right.png"
                            |frog4.direction == Up -> "frog-up.png"
                            |otherwise -> "frog-down.png"
              in container w h middle <| collage gameWidth gameHeight
                  [ rect gameWidth gameHeight |> filled grey
                  , rect gameWidth 20 |> filled yellow
                  , rect gameWidth fourthHeight |> filled green
                                                |> move (0, fourthHeight*1.5)
                  , rect gameWidth fourthHeight |> filled green
                                                |> move (0, -fourthHeight*1.5)
                  , circle 30 |> filled brown
                              |> move (0, fourthHeight*1.5)
                  , circle 30 |> filled brown
                              |> move (halfWidth/3, fourthHeight*1.5)
                  , circle 30 |> filled brown
                              |> move (halfWidth*2/3, fourthHeight*1.5)
                  , circle 30 |> filled brown
                              |> move (-halfWidth/3, fourthHeight*1.5)
                  , circle 30 |> filled brown
                              |> move (-halfWidth*2/3, fourthHeight*1.5)
                  , toForm (image 20 20 src) |> makefrog frog
                  , toForm (image 20 20 src1) |> makefrog frog1
                  , toForm (image 20 20 src2) |> makefrog frog2
                  , toForm (image 20 20 src3) |> makefrog frog3
                  , toForm (image 20 20 src4) |> makefrog frog4
                  , toForm (image 40 30 "blue-car-left.png") |> make car
                  , toForm (image 40 30 "white-car-left.png") |> make car1
                  , toForm (image 40 30 "yellow-car-left.png") |> make car2
                  , toForm (image 40 30 "red-car-left.png") |> make car3
                  , toForm (image 40 30 "green-car-right.png") |> make card
                  , toForm (image 40 30 "aqua-car-right.png") |> make card1
                  , toForm (image 40 30 "white-car-right.png") |> make card2
                  , toForm (image 40 30 "yellow-car-right.png") |> make card3
                  ]

        
main = lift2 display Window.dimensions gameState