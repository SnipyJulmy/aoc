module Main where

import System.Environment

main = do
  args <- getArgs
  content <- fmap words (readFile (args !! 0))
  let rotations = map parse content
  let (_,score1) = foldl func (50, 0) rotations
  let (_,score2) = foldl func2 (50, 0) rotations
  putStrLn ("Part 1: " ++ (show score1))
  putStrLn ("Part 2: " ++ (show score2))

func :: (Integer,Integer) -> Integer -> (Integer, Integer)
func (dial, acc) elt = (new_dial, if new_dial == 0 then acc + 1 else acc)
  where new_dial = mod ((dial + elt) + 100) 100

func2 :: (Integer,Integer) -> Integer -> (Integer,Integer)
func2 (dial,acc) elt = (new_dial, new_acc)
  where s = dial + elt
        new_dial = mod (s + 100) 100
        new_acc = if (s <= 0) || (s > 99) then acc + 1 else acc

parse :: String -> Integer
parse ('R' : value) = read value :: Integer
parse ('L' : value) = -(read value :: Integer)


