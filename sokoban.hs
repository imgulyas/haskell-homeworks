{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = drawingOf pictureOfMaze

tileSize :: Double
tileSize = 0.7

data Coord = C Integer Integer

wall, ground, storage, box, player :: Picture
wall =    colored black $ solidRectangle tileSize tileSize
ground =  colored blue  $ solidRectangle tileSize tileSize
storage = colored green $ solidRectangle tileSize tileSize
box =     colored red   $ solidRectangle tileSize tileSize
player = undefined

data Tile = Wall | Ground | Storage | Box | NoneTile

drawTile :: Tile -> Picture
drawTile Wall     = wall
drawTile Ground   = ground
drawTile Storage  = storage
drawTile Box      = box
drawTile NoneTile = blank

pictureOfMaze :: Picture
pictureOfMaze = concatPictures $ map
                (\(C x y) ->
                  translated (fromIntegral x * tileSize) (fromIntegral y * tileSize)
                    $ drawTile $ maze (C x y))
                                coordinates
        where
          coordinates = [ C x y | x<- [(-10), (-9) .. 10], y <- [(-10), (-9) .. 10]]

          concatPictures :: [Picture] -> Picture
          concatPictures = foldl (&) blank

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = NoneTile
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Box
  | x >= -2 && y == 0        = Storage
  | otherwise                = Ground
