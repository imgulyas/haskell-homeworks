{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise2

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, midCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
midCircle c = colored c (translated 0    0   (solidCircle 1))
topCircle c = colored c (translated 0   2.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

data TrafficLightState = Green | Yellow | Red | RedAndYellow | Black
                            deriving Eq

trafficLight :: TrafficLightState -> Picture
trafficLight Green        = botCircle green & midCircle black  & topCircle black & frame
trafficLight Yellow       = botCircle black & midCircle yellow & topCircle black & frame
trafficLight Red          = botCircle black & midCircle black  & topCircle red   & frame
trafficLight RedAndYellow = botCircle black & midCircle yellow & topCircle red   & frame
trafficLight Black        = botCircle black & midCircle black  & topCircle black & frame

stateFunctionMaker :: [(TrafficLightState, Double)] ->  (Double -> TrafficLightState)
stateFunctionMaker stateDurations = state
          where
                times      = map snd stateDurations
                timeranges = zipWith (\d xs -> d + sum(xs)) times (prefixes times)
                statesWithRange = zipWith (,) (map fst stateDurations) timeranges

                prefixes :: [a] -> [[a]]
                prefixes list = zipWith (\n _ -> take n list) [0..] list

                state' :: Double -> [(TrafficLightState, Double)] -> TrafficLightState
                state' t []     = if stateDurations == [] then Black
                                  else state' (t - (sum times)) statesWithRange
                state' t (x:xs) = if t < snd x then fst x else state' t xs

                state t = state' t statesWithRange

durations :: [(TrafficLightState, Double)]
durations = [(Green, 1), (Yellow, 0.2), (Red, 1), (RedAndYellow, 0.2)]

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficLight . stateFunctionMaker durations

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Integer -> Picture -> Picture
tree 0 blossom = blossom
tree n blossom = path [(0,0),(0,1)]
      & translated 0 1
      (rotated (pi/10) (tree (n-1) blossom) & rotated (- pi/10) (tree (n-1) blossom))

blossomAnim :: Double -> Double -> Double -> Picture
blossomAnim maxSize fullBloom t = if t < fullBloom
                                  then colored yellow $ solidCircle $ maxSize * t / fullBloom
                                  else colored yellow $ solidCircle maxSize

animatedTree :: Double -> Picture
animatedTree t = tree 8 (blossomAnim 0.3 10 t)

exercise2 :: IO ()
exercise2 = animationOf animatedTree

-- Exercise 3

wall, ground, storage, box :: Picture
wall =    undefined
ground =  undefined
storage = undefined
box =     undefined

drawTile :: Integer -> Picture
drawTile = undefined


pictureOfMaze :: Picture
pictureOfMaze = undefined

exercise3 :: IO ()
exercise3 = undefined

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
