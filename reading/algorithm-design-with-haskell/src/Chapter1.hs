module Chapter1 where

{-| Exercise 1.3: 

    Examples:

    >>> wrap 5
    [5]
-}
wrap :: a -> [a]
wrap x = [x]

-- | Exercise 1.3: unwrap
--
--   >>> unwrap [5]
--   5
unwrap :: [a] -> a
unwrap [x] = x

-- | Exercise 1.3: single
--
--   >>> single [5]
--   True
--   >>> single [5, 4]
--   False

single :: [a] -> Bool 
single [_] = True 
single  _  = False

-- | Exercise 1.4: myReverse
--
--   >>> myReverse [1, 2, 3]
--   [3,2,1]

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- | Exercise 1.5: myMap
-- 
--   >>> myMap (+ 1) [1, 2, 3]
--   [2,3,4]
myMap f = foldr (\a b -> f a : b) []

-- | Exercise 1.5: myFilter
-- 
--  >>> myFilter (/= 0) [1, 0, 2, 0, 3]
--  [1,2,3]

myFilter f = foldr (\a b -> if f a then a : b else b) []

-- | Exercise 1.6: fold f e . filter g == foldFilter f e g
-- 
--   >>> foldFilter (+) 0 odd [1,2,3,4,5]
--   9

foldFilter f e g = foldr (\a b -> if g a then f a b else b) e

-- | Exercise 1.7 : myTakeWhile
--
--   >>> myTakeWhile odd [1,1,3,5,2,3]
--   [1,1,3,5]
--   >>> myTakeWhile (< 10) [1..]
--   [1,2,3,4,5,6,7,8,9]

myTakeWhile f = foldr (\a b -> if f a then a : b else []) []