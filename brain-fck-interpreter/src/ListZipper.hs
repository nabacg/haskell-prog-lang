module ListZipper (ListZipper, fromList, zipperOf, moveLeft, moveRight, getCurr, setCurr, incCurr, decCurr) where

import Data.List(intercalate)

-- https://wiki.haskell.org/Zipper
-- https://en.wikipedia.org/wiki/Zipper_(data_structure)
-- https://gist.github.com/sherubthakur/16a784e61efbe54d885ad60c6e18f254


data ListZipper x = ListZipper [x] x [x]


instance Show a => Show (ListZipper a) where
  show (ListZipper ls c rs) = showLeft ++ showCurr ++ showRight
    where
      showCurr  = " " ++ show c ++ " "
      showLeft  = "...," ++ intercalate ","  (reverse . map show $ take 10 ls) ++ "]"
      showRight = "[" ++ intercalate ","  (map show $ take 10 rs) ++ ", ..."

zipperOf :: a -> ListZipper a
zipperOf x = ListZipper (repeat x) x (repeat x)


fromList :: a -> [a] -> ListZipper a
fromList a [] = zipperOf a
fromList a (x:xs) = ListZipper (repeat a) x xs


moveRight :: ListZipper a -> ListZipper a
moveRight (ListZipper ls c []) = error "Can't move to the right, run out of list"
moveRight (ListZipper ls c (r:rs)) = ListZipper (c:ls) r rs


moveLeft :: ListZipper a -> ListZipper a
moveLeft (ListZipper [] _ _) = error "Can't move to the left, run out of list"
moveLeft (ListZipper (l:ls) c rs) = ListZipper ls l (c:rs)

getCurr :: ListZipper a -> a
getCurr (ListZipper _ c _) = c

setCurr :: a -> ListZipper a -> ListZipper a
setCurr x (ListZipper ls _ rs) = ListZipper ls x rs

incCurr :: ListZipper Int -> ListZipper Int
incCurr (ListZipper ls c rs) = ListZipper ls ((c+1) `mod` 256) rs


decCurr :: ListZipper Int -> ListZipper Int
decCurr (ListZipper ls c rs) = ListZipper ls ((c-1) `mod` 256) rs


-- Tests
-- zl == moveLeft $moveLeft $ moveRight $ moveRight zl
