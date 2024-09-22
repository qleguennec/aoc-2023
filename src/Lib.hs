{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( spaces',
    concatNums,
    iff,
    ifff,
    (/>>),
    (||>),
    updatev,
    toMap,
    fix,
    dimlike,
    partition,
    repeatedly,
    nfix,
    hmap,
    fixOn,
    normals,
    RangeSplit,
    splitRange,
    splitRangeT,
    cross,
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Biapplicative
import Data.Bifunctor
import Data.Char
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.Range
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear.V2
import Text.Parsec hiding (spaces, (<|>))

spaces' :: (Stream s m Char) => ParsecT s u m ()
spaces' = skipMany (char ' ')

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' p xs = case f xs of
  (a, []) -> [a]
  (a, b) -> a : groupBy' p b
  where
    f [] = ([], [])
    f (x : xs) = ([x], (++) []) <*> f' x xs
    f' _ [] = ([], [])
    f' c (x : xs) = if p c x then f (x : xs) else ([], x : xs)

concatNums :: (Foldable t) => t Int -> Int
concatNums = read . foldl ((. show) . (++)) ""

-- subs = List.unfoldr sub

-- sub [] = Nothing
-- sub (_ : []) = Nothing
-- sub (x : y : xs) = Just (y - x, y : xs)

iff :: Bool -> a -> a -> a
iff True x _ = x
iff False _ y = y

ifff :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
ifff p f g x = iff (p x) (f x) (g x)

(/>>) :: (Alternative m) => (a -> Bool) -> (a -> m b) -> a -> m b
(/>>) p = ifff p (const empty)

infixr 5 />>

(||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(||>) p p' x = p x || p' x

infixr 5 ||>

updatev :: Int -> (a -> a) -> Vector a -> Vector a
updatev i f v = v V.// [(i, f (v V.! i))]

toMap :: [[a]] -> HashMap (V2 Int) a
toMap = M.fromList . concat . zipWith (\y -> map (\(x, c) -> (V2 x y, c))) [0 ..] . map (zip [0 ..])

fixOn :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixOn p f a = let a' = f a in if p a a' then a' else fixOn p f a'

fix :: (Eq a) => (a -> a) -> a -> a
fix = fixOn (==)

repeatedly :: ([a] -> (b, [a])) -> [a] -> [b]
repeatedly f [] = []
repeatedly f as = let (b, as') = f as in b : repeatedly f as'

partition :: Int -> [a] -> [[a]]
partition i xs | i <= 0 = error $ "chunksOf, number must be positive, got " ++ show i
partition i xs = repeatedly (splitAt i) xs

dimlike :: [[a]] -> [b] -> [[b]]
dimlike ys = partition (length (head ys))

nfixH :: (Hashable a) => HashMap a Int -> Int -> (a -> a) -> a -> ([a], [a])
nfixH m n f a =
  case M.lookup a m of
    Just i ->
      join bimap (map fst . L.sortOn snd)
        . L.partition (\(k, a) -> a < i)
        . M.toList
        $ m
    Nothing -> let a' = f a in nfixH (M.insert a n m) (succ n) f a'

nfix :: (Hashable a) => (a -> a) -> a -> ([a], [a])
nfix = nfixH M.empty 0

hmap :: [[b]] -> HashMap (Int, Int) a -> [[a]]
hmap ys = dimlike ys . map snd . L.sortOn fst . M.toList

normals :: Int -> Int -> [((Int, Int), (Int, Int))]
normals w h = concat [topRow, bottomRow, leftCol, rightCol, corners]
  where
    topRow = [((0, x), (1, 0)) | x <- [1 .. w - 2]]
    bottomRow = [((h - 1, x), (-1, 0)) | x <- [1 .. w - 2]]
    leftCol = [((y, 0), (0, 1)) | y <- [1 .. h - 2]]
    rightCol = [((y, w - 1), (0, -1)) | y <- [1 .. h - 2]]
    corners =
      [ ((0, 0), (1, 0)),
        ((0, 0), (0, 1)),
        ((0, w - 1), (1, 0)),
        ((0, w - 1), (0, -1)),
        ((h - 1, 0), (-1, 0)),
        ((h - 1, 0), (0, 1)),
        ((h - 1, w - 1), (-1, 0)),
        ((h - 1, w - 1), (0, -1))
      ]

area :: (Integral a, Fractional b) => [V2 a] -> b
area =
  (* 0.5)
    . fromIntegral
    . abs
    . sum
    . map (\(a, b) -> (a ^. _x) * (b ^. _y) - (a ^. _y) * (b ^. _x))
    . ap zip tail

data RangeSplit = RangeSplit {_lt :: Range Int, _gt :: Range Int}
  deriving (Eq, Show)

makeLenses ''RangeSplit

splitRange :: Int -> Range Int -> Maybe RangeSplit
splitRange x r = case difference [r] [x +=+ x] of
  [r0, r1] -> Just $ RangeSplit {_lt = r0, _gt = r1}
  _ -> Nothing

splitRangeT :: Ordering -> Int -> Range Int -> Maybe (Range Int, Range Int)
splitRangeT order x = (((,) <$> view (ol order) <*> head . union [x +=+ x] . return . view (rol order)) <$>) . splitRange x
  where
    ol GT = gt
    ol LT = lt
    rol GT = lt
    rol LT = gt

cross :: (Num a) => V2 a -> a
cross (V2 a b) = a * b