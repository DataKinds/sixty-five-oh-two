module Main where

import SixtyFiveOhTwo.Instruction
import Control.Monad.State
import qualified Data.ByteString as B

accumulatorLoadNStore :: Instruction
accumulatorLoadNStore = do
    lda (Immediate 0x10)
    sta (Absolute 0x0200)
    rts (Implied)

myProgram :: Instruction
myProgram = do
    define "accumulatorLoadNStore" accumulatorLoadNStore
    call "accumulatorLoadNStore"

main :: IO ()
main = B.putStr $ runInstructions myProgram
