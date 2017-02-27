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
streamInterleave (Cons x xs) zs = Cons x $ streamInterleave zs xs

nats :: Stream Integer
nats = streamIterate (+ 1) 0

data Supply s a = S (Stream s -> (a, Stream s))

get :: Supply s s
get = S $ \(Cons elem (stream)) -> (elem, stream)

pureSupply :: a -> Supply s a
pureSupply a = S $ \stream -> (a, stream)

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S t) = S $ (\(first, second) -> (f first, second)) . t

--mapSupply f (S t) = go
--  where go xs = let (a, xs') = t xs
--                in ((f a), xs')

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
-- mapSupply2 f (S t1) (S t2) = S $ (\(first, second) -> (f first, second)) . t
mapSupply2 f (S t1) (S t2) = S go
        where
              go xs = let (a, xs')  = t1 xs
                          (b, xs'') = t2 xs'
                       in (f a b, xs'')

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S t) f = S $ go
  where
    go s = let (a', s') = t s
               (S g) = f a'
            in  g s'

runSupply :: Stream s -> Supply s a -> a
runSupply s (S f) = fst $ f s

instance Functor (Supply s) where
    fmap = mapSupply

instance Applicative (Supply s) where
    pure = pureSupply
    (<*>) = mapSupply2 id

instance Monad (Supply s) where
    return = pureSupply
    (>>=) = bindSupply

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
  where
    go :: Tree a -> Supply s (Tree s)
    go node@(Node left right) = go left >>=


    go (Leaf l) = pureSupply l