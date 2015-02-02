import Data.List
import Data.Char

type Location = String
type Direction = String
type Thing = String
type Response = String


type PathMap = [((Location, Direction), Location)]
paths :: PathMap
paths = [
    (("bedroom", "n"), "den"),
    (("den", "s"), "bedroom"),
    (("bedroom", "d"), "bed"),
    (("bed", "u"), "bedroom")
    ]


type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations = [
    --("flyswatter", "den"),
    --("fly", "bedroom"),
    ("den_light_switch", "den"),
    ("bedroom_light_switch", "bedroom"),
    ("myself", "bedroom"),
    ("fly_state", "alive"),
    ("bedroom_light", "on"),
    ("den_light", "on")
    ]


type World = (PathMap, LocationMap, Response)
world :: IO (PathMap, LocationMap, Response)
world = return (paths, locations, "")

main :: IO (String)
main = do
    putStrLn "\nWelcome to the sleepy game!\n"
    putStrLn instructions
    play_game $ return (paths, locations, "")
    return "Goodbye!"
    
instructions =
    "Enter commands using one or two words.\n" ++
    "Available commands are:\n" ++
    "main               -- to start the game.\n" ++
    "n  s  e  w  u  d   -- to go in that direction.\n" ++
    "take object        -- to pick up the named object.\n" ++
    "drop object        -- to put down the named object.\n" ++
    "use object         -- to manipulate an object.\n" ++
    "look               -- to look around you again.\n" ++
    "on  off            -- to control the room lights.\n" ++
    "sleep              -- to try to go to sleep.\n" ++
    "instructions       -- to see this message again.\n" ++
    "quit               -- to end the game and quit."


play_game :: IO (World) -> IO (World)
play_game world = do
    (paths, locations, response) <- world
    putStrLn response
    putStrLn ""
    if response == "Ahhh...you (yawn) made...it...zzzzzzzz."
        then return (paths, locations, "Quitting.")
        else do
            putStr "command> "
            command <- getLine
            if command == "quit"
                then return (paths, locations, "Quitting.")
                else  play_game $ return (do_command command paths locations)

can_move :: Location -> Direction -> PathMap -> LocationMap -> Bool
can_move "bedroom" "n" _ locations = get "bedroom_light" locations == "on"
can_move from direction paths _ =
    elem (from, direction) keys 
    where (keys, _) = unzip paths

cannot_move_because :: Location -> Direction -> Response
cannot_move_because "bedroom" "n" = "You trip over something in the dark."
cannot_move_because _ _ = "You can't go that way."

move :: Location -> Direction -> PathMap -> Location
move from direction paths = get (from, direction) paths

do_command :: String -> PathMap -> LocationMap -> World
do_command "n" paths locations = go "n" paths locations
do_command "e" paths locations = go "e" paths locations
do_command "s" paths locations = go "s" paths locations
do_command "w" paths locations = go "w" paths locations
do_command "u" paths locations = go "u" paths locations
do_command "d" paths locations = go "d" paths locations
do_command "look" paths locations = look paths locations
do_command "on" paths locations = take_on paths locations
do_command "off" paths locations = take_off paths locations
do_command "sleep" paths locations = sleep paths locations
do_command "instructions" paths locations = instructionshelper paths locations
do_command "quit" paths locations = (paths, locations, "quit")
do_command "dump" paths locations =
    (paths, locations, "paths = " ++ show paths ++ "\nlocations = " ++ show locations)
do_command cmd paths locations = do_command_2 cmd paths locations

do_command_2 :: String -> PathMap -> LocationMap -> World
do_command_2 cmd paths locations
    | isPrefixOf "take " cmd =
          game_take (tail $ snd $ span isLetter cmd) paths locations
    | isPrefixOf "drop " cmd =
          game_drop (tail $ snd $ span isLetter cmd) paths locations
    | isPrefixOf "use " cmd =
          use (tail $ snd $ span isLetter cmd) paths locations
    | otherwise = (paths, locations, "I don't understand: " ++ cmd)

take_on paths locations =
    let here = get "myself" locations
    in if here == "bedroom"
        then let bedroom_state = get "bedroom_light" locations
             in if bedroom_state == "on"
                then (paths, locations, "The lights are already on.")
                else (paths, (put "bedroom_light" "on" locations), "The room lights come on.")
        else if here == "bed"
            then (paths, locations, "You can't reach the light switch from here.")
            else let den_state = get "den_light" locations
                 in if den_state == "on"
                    then (paths, locations, "The lights are already on.")
                    else (paths, (put "den_light" "on" locations), "The room lights come on.")

take_off paths locations =
    let here = get "myself" locations
    in if here == "bedroom"
        then let bedroom_state = get "bedroom_light" locations
             in if bedroom_state == "off"
                then (paths, locations, "The lights are already off.")
                else (paths, (put "bedroom_light" "off" locations), "It is now dark in here.")
        else if here == "bed"
            then (paths, locations, "You can't reach the light switch from here.")
            else let den_state = get "den_light" locations
                 in if den_state == "off"
                    then (paths, locations, "The lights are already off.")
                    else (paths, (put "den_light" "off" locations), "It is now dark in here..")

game_take :: Thing -> PathMap -> LocationMap -> World          
game_take thing paths locations =
    let here = get "myself" locations
        there = get thing locations
    in if here == there
       then if thing == "bedroom_light_switch" 
            then (paths, locations, "It's firmly embedded in the wall!")
            else if thing == "fly"
                then (paths, locations, "It's too fast for you!")
                else if thing == "den_light_switch"
                    then (paths, locations, "It's firmly embedded in the wall!")
                    else (paths, (put thing "holding" locations), "OK, taken.")
       else if there == "holding"
            then (paths, locations, "You are already holding it.")
            else (paths, locations, "I don't see it here.")
        
game_drop :: Thing -> PathMap -> LocationMap -> World          
game_drop thing paths locations = --(paths, locations, "filler")
    let here = get "myself" locations
        there = get thing locations
    in if there == "holding"
        then (paths, (put thing here locations), "OK, dropped.")
        else (paths, locations, "You aren't holding it.")

use thing paths locations = 
    let here = get "myself" locations
    in if thing == "bed"
        then if here == "bedroom"
             then go "d" paths locations
             else if here == "bed"
                then (paths, locations, "You are already in the bed.")
                else (paths, locations, "It's in the bedroom.")
        else if thing == "bedroom_light_switch"
            then if here == "bedroom"
                 then let bedroom_state = get "bedroom_light" locations
                      in if bedroom_state == "on"
                         then take_off paths locations
                         else take_on paths locations
                 else (paths, locations, "It's in the bedroom.")
        else if thing == "den_light_switch"
            then if here == "den"
                 then let den_state = get "den_light" locations
                      in if den_state == "on"
                         then take_off paths locations
                         else take_on paths locations
                 else (paths, locations, "It's in the den.")
        else if thing == "flyswatter"
            then let status = get "flyswatter" locations
                 in if status == "holding"
                    then swat paths locations
                    else (paths, locations, "You do not have flyswatter.")
            else (paths, locations, "The object does not exist.")


swat paths locations =
    let here = get "myself" locations
        bedroom_state = get "bedroom_light" locations
        den_state = get "den_light" locations
        fly_state = get "fly_state" locations
        fly_location = get "fly" locations 
    in if here /= fly_location
        then (paths, locations, "You swish the flyswatter through the air.")
        else if fly_state == "die"
            then (paths, locations, "He's dead, Jim.")
            else if here == "bedroom" && bedroom_state == "off"
                then (paths, locations, "You flail aimlessly in the dark!")
                else if here == "den" && den_state == "off"
                    then (paths, locations, "You flail aimlessly in the dark!")
                    else if bedroom_state == "on" && den_state == "on"
                        then if here == "bedroom"
                            then (paths, (put "fly" "den" locations), "The fly escapes into the other room.")
                            else (paths, (put "fly" "bedroom" locations), "The fly escapes into the other room.")
                    else (paths, (put "fly_state" "die" locations), "Success! You killed that pesky fly!")

go :: String -> PathMap -> LocationMap -> World
go direction paths locations = do
    let my_location = get "myself" locations
    if can_move my_location direction paths locations
        then do
            let new_location = move my_location direction paths
            let new_locations = put "myself" new_location locations
            let response = describe new_location new_locations
            (paths, new_locations, response)
        else (paths, locations, cannot_move_because my_location direction)

look :: PathMap -> LocationMap -> World
look paths locations =
    if things == []
        then (paths, locations, describe my_location locations)
        else (paths, locations, describe my_location locations ++ "\n\n" ++ things)
    where my_location = get "myself" locations
          things = items_here locations

instructionshelper paths locations =
    (paths, locations, instructions)
 
sleep paths locations =
    let here = get "myself" locations
        bedroom_light = get "bedroom_light" locations
        den_light = get "den_light" locations
        flyswatter_status = get "flyswatter" locations
        fly_status = get "fly_state" locations
    in if here /= "bed"
        then (paths, locations, "You find it hard to sleep standing up.")
        else if bedroom_light == "on"
            then (paths, locations, "You can't get to sleep with the light on.")
            else if den_light == "on"
                then (paths, locations, "The light from the den is keeping you awake.")
                else if flyswatter_status == "holding" || flyswatter_status == "bed"
                    then (paths, locations, "What? Sleep with a dirty old flyswatter?")
                    else if fly_status == "alive"
                        then (paths, (put "flyswatter" "den" (put "fly" "bedroom" locations)), "As soon as you start to doze off, a fly lands on your face and wakes you up again.")
                        else (paths, locations, "Ahhh...you (yawn) made...it...zzzzzzzz.")

inventory :: LocationMap -> Response
inventory locations =
    let my_stuff = [thing | (thing, "holding") <- locations]
    in if my_stuff == []
        then "You aren't holding anything."
        else intercalate ", " my_stuff

items_here :: LocationMap -> Response
items_here locations =
    let here = get "myself" locations
        things = ["There is a " ++ thing ++ " here." |
                  (thing, place) <- locations, place == here, thing /= "myself"]
    in intercalate "\n" things

-- "get" finds the value of a key in a (key, value) list
get :: Eq a => a -> [(a, String)] -> String
get value list = case lookup value list of
                     Just result -> result
                     Nothing -> "Not found."

put :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
put key value list =
    let without = filter (\(x, y) -> x /= key) list
    in (key, value) : without

describe :: Location -> LocationMap -> String
describe new_location locations =
    let here = get "myself" locations
        bedroom_light = get "bedroom_light_switch" locations
        den_light = get "den_light_switch" locations
    in describe_helper here bedroom_light den_light locations 

describe_helper :: Location -> String -> String -> LocationMap -> String
describe_helper "bedroom" "off" _ locations = description "bedroom_off"
describe_helper "den" _ "off" locations = description "den_off"
describe_helper here _ _ locations = description here

description :: Location -> String
description "bedroom" =
    "You are in a bedroom with a large, comfortable bed.\n" ++
    "It has been a long, tiresome day, and you would like\n" ++
    "nothing better than go to sleep."

description "bedroom_off" =
    "You are in your bedroom. It is nice and dark."

description "bed" =
    "You are in bed, and it feels great!\n"

description "den" =
    "You are in your den. There is a lot of stuff here.\n" ++
    "but you are too sleepy to care about most of it." 

description "den_off" =
    "You are in your den. It is dark."

description someplace = someplace ++ ", and you can't see anything."