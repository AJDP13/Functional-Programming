-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

module Assignment1 ( put,
                     moveLeft,
                     moveRight,
                     moveUp,
                     moveDown,
                     Grid(..),
                     GridWithAPointer(..),
                     putTatamiDown,
                     putTatamiUp,
                     putTatamiLeft,
                     putTatamiRight,
                     cover
             ) where

import Data.Char (isLetter)

-- these two function are to correctly measure the width of an entry of a grid, 
-- i.e. so that the width of "\ESC[44m55\ESC[0m" ignored the escape sequences
stripANSI :: String -> String
stripANSI [] = []
stripANSI ('\ESC':'[':xs) = stripANSI (drop 1 (dropWhile (not . isLetter) xs))
stripANSI (x:xs) = x : stripANSI xs

visibleLength :: String -> Int
visibleLength = length . stripANSI

newtype Grid a = Grid { grid :: [[a]] } deriving Eq

instance (Show a) => Show (Grid a) where
  show (Grid g)
    | null g = ""
    | otherwise = unlines (map showRow g)
    where
      strGrid = map (map show) g
      colWidths = [maximum (map visibleLength col) | col <- transpose strGrid]
      showRow row = unwords [padRight w s | (w, s) <- zip colWidths (map show row)]
      padRight n s = s ++ replicate (n - visibleLength s) ' '

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)


newtype GridWithAPointer a = GridWithAPointer (Grid a, [a], a, [a], Grid a)
  deriving Eq


---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- TASK 1
---------------------------------------------------------------------------------

instance (Show a) => Show (GridWithAPointer a) where
     show (GridWithAPointer (topGrid, left, focus, right, bottomGrid)) = 
         let topStr = show topGrid
             leftStr = unwords (map show (reverse left))
             focusStr = show focus
             rightStr = unwords (map show right)
             bottomStr = show bottomGrid
         in topStr ++ leftStr ++ " " ++ "\ESC[44m" ++ focusStr ++ "\ESC[0m" ++ " " ++ rightStr ++ "\n" ++ bottomStr

---------------------------------------------------------------------------------
-- TASK 2
---------------------------------------------------------------------------------

put :: a -> GridWithAPointer a -> GridWithAPointer a
put newVal (GridWithAPointer (topGrid, left, focus, right, bottomGrid)) = 
  GridWithAPointer (topGrid, left, newVal, right, bottomGrid)

moveLeft :: GridWithAPointer a -> GridWithAPointer a
moveLeft (GridWithAPointer (topGrid, left, focus, right, bottomGrid)) = 
  GridWithAPointer (topGrid, (tail left), (head left), ([focus] ++ right), bottomGrid)


moveRight :: GridWithAPointer a -> GridWithAPointer a
moveRight (GridWithAPointer (topGrid, left, focus, right, bottomGrid)) = 
  GridWithAPointer (topGrid, [focus] ++ left, head right, tail right, bottomGrid)

moveUp :: GridWithAPointer a -> GridWithAPointer a
moveUp (GridWithAPointer (Grid topGrid, left, focus, right, Grid bottomGrid)) = 
  let newTopGrid = Grid (reverse (tail (reverse topGrid)))
      newLeft = reverse (take (length left) (last topGrid))
      newFocus = (last topGrid) !! (length left)
      newRight = drop (length left + 1) (last topGrid)
      newBottomGrid = Grid ([(reverse left) ++ [focus] ++ right] ++ bottomGrid)
  in GridWithAPointer (newTopGrid, newLeft, newFocus, newRight,  newBottomGrid)

moveDown :: GridWithAPointer a -> GridWithAPointer a
moveDown (GridWithAPointer (Grid topGrid, left, focus, right, Grid bottomGrid)) = 
  let newTopGrid = Grid (topGrid ++ [(reverse left) ++ [focus] ++ right])
      newLeft = reverse (take (length left) (head bottomGrid))
      newFocus = (head bottomGrid) !! (length left)
      newRight = drop (length left + 1) (head bottomGrid)
      newBottomGrid = Grid (tail bottomGrid)
  in GridWithAPointer (newTopGrid, newLeft, newFocus, newRight, newBottomGrid)


---------------------------------------------------------------------------------
-- TASK 3
---------------------------------------------------------------------------------

putTatamiUp :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiUp num = moveDown . put num . moveUp . put num

putTatamiDown :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiDown num = moveUp . put num . moveDown . put num

putTatamiRight :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiRight num = moveLeft . put num . moveRight . put num

putTatamiLeft :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiLeft num = moveRight . put num . moveLeft . put num


---------------------------------------------------------------------------------
-- TASK 4
---------------------------------------------------------------------------------

cover :: GridWithAPointer Integer -> GridWithAPointer Integer
cover = undefined


t = GridWithAPointer (Grid [[0,0,0,0]], [0], 0, [0,0], Grid [[0,0,0,0]])