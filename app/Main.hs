module Main where

main :: IO ()
main = do
  putStrLn "The great tree!"

data Tree a = Node a (Tree a) (Tree a)
      | Nil
      deriving(Show, Eq)

put :: (Ord a) => Tree a -> a -> Tree a
put Nil obj = Node obj Nil Nil
put (Node val left right) obj
  | obj < val = Node val (put left obj) right
  | obj > val = Node val left (put right obj)
  | otherwise = Node val left right

find :: (Ord a) => Tree a -> a -> Bool
find Nil _ = False
find (Node obj left right) a
  | obj == a = True
  | obj > a  = find left a
  | otherwise = find right a

traversal :: Tree a -> [a]
traversal Nil = []
traversal (Node a left right) =
  traversal left ++ [a] ++ traversal right
