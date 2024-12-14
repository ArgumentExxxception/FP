module StackOperations (pop , push)
where 

import Control.Monad.State
import Data.Maybe (fromMaybe)

type Stack = [Int]

pop :: State Stack (Maybe Int)
pop = state $ \s -> case s of
    []     -> (Nothing, [])
    (x:xs) -> (Just x, xs)

push :: Int -> State Stack ()
push x = modify (x:)