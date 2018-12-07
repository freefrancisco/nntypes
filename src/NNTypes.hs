module NNTypes where

import Data.List (unfoldr, mapAccumR, mapAccumL)


encodingRnn :: (s -> x -> s) -> s -> [x] -> s
encodingRnn a s = foldl a s

generatingRnn :: (s -> Maybe (y, s)) -> s -> [y]
generatingRnn a s = unfoldr a s

generalRnn :: (s -> x -> (s, y)) -> s -> [x] -> (s, [y])
generalRnn a s = mapAccumR a s

-- reverse rnn
generalRnn' :: (s -> x -> (s, y)) -> s -> [x] -> (s, [y])
generalRnn' a s = mapAccumL a s

bidirectionalRnn :: (s -> x -> (s, y)) -> s -> -- forward rnn
                    (s -> x -> (s, y)) -> s -> -- reverse rnn
                    [x] -> ((s, s), [(y,y)])   -- totals and zipped sequences
bidirectionalRnn a s a' s' xs = ((final, final'), zip ys ys') where
    (final,  ys)  = generalRnn  a  s  xs
    (final', ys') = generalRnn' a' s' xs
