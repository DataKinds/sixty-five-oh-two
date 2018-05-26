module Main where

import DSL.SixtyFiveOhTwo
import Control.Monad.State
import qualified Data.ByteString as B
import Data.Int

test1 :: Instruction
test1 = do
    lda (Immediate 0xFF)
    sta (ZeroPage 0x00)
    lda (Immediate 0x00)
    adc (Immediate 0x01)
    cmp (ZeroPage 0x00)
    bne (Relative (-0x03 :: Int8))


test2f :: Instruction
test2f = do
    lda (Immediate 0x10)
    sta (Absolute 0x0200)
    rts (Implied)

test2 :: Instruction
test2 = do
    define "accumulatorLoadNStore" test2f
    call "accumulatorLoadNStore"

test3f2 :: Instruction
test3f2 = replicateM_ 10 (inc (Accumulator))

test3f1 :: Instruction
test3f1 = do
    lda (Immediate 0x02)
    define "addIt" test3f2

test3 :: Instruction
test3 = do
    define "loadIt" test3f1
    call "loadIt"
    call "addIt"

main :: IO ()
main = do
    putStrLn "test one: simple program"
    putStrLn "========================"
    print $ execState test1 emptyState
    putStrLn ""
    putStrLn "test two: simple function"
    putStrLn "========================="
    print $ execState test2 emptyState
    putStrLn ""
    putStrLn "test two: nested function"
    putStrLn "========================="
    print $ execState test3 emptyState
    putStrLn ""
