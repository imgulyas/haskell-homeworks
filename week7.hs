data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons head stream) = head : streamToList stream

instance Show a => Show (Stream a) where
  show stream =  let
      streamStr = init $ tail $ show $ take 20 $ streamToList stream
      in
      "<" ++ streamStr ++ ", ... >"

streamRepeat :: a -> Stream a
streamRepeat elem = Cons elem (streamRepeat elem)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons elem stream) = Cons (f elem) (streamMap f stream)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate rule head = let
        restOfStream :: (a -> a) -> a -> Stream a
        restOfStream r elem = Cons (r elem) (restOfStream r $ r elem)
        in
        Cons head (restOfStream rule head)

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) (Cons y ys) = Cons x $ Cons y $ streamInterleave xs ys

nats :: Stream Integer
nats = streamIterate (+ 1) 0

--  the nth element in the stream (assuming the first element corresponds to n=1)
--  is the largest power of 2 which evenly divides n
ruler :: Stream Integer
ruler = let
  powers = map (2^) [0 ..]
  toRuler :: Integer -> Integer
  toRuler n = last $ filter (\m -> mod n m == 0) $ filter (n >=) powers
  in
  streamMap toRuler $ streamIterate (+ 1) 1