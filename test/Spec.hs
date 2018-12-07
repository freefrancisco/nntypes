import Test.Hspec
import Test.QuickCheck

import NNTypes

xs = ["a", "b", "c", "d"]
s0 = "S0"
generalLayer :: String -> String -> (String, String)
generalLayer s x = (s ++ x, "x: " ++ x ++ ", s: " ++ s)

printResults title (final, ys) = do
    putStrLn title
    putStrLn "final State: "
    print final
    putStrLn "ys: "
    print ys

main :: IO ()
main = hspec $ do
    describe "encodingRnn" $ do
        it "runs and produces a final state" $ do
            let layer s x = sf where (sf, y) = generalLayer s x
            let final = encodingRnn layer s0 xs
            printResults "Encoding RNN" (final, ["Not applicable"])

    describe "generalRnn" $ do
        it "runs forward" $ do
            let (final, ys) = generalRnn generalLayer s0 xs
            printResults "Forward RNN" (final, ys)

    describe "generalRnn'" $ do
        it "runs backwards" $ do
            let (final, ys) = generalRnn' generalLayer s0 xs
            printResults "Backwards RNN" (final, ys)

    describe "bidirectionalRnn" $ do
        it "zips forward and backward runs" $ do
            let (finals, ys) = bidirectionalRnn generalLayer s0 generalLayer s0 xs
            printResults "Bidirectional RNN" (finals, ys)
