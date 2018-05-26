# A 65C02 Assembly DSL for Haskell

![Example image](https://raw.githubusercontent.com/Aearnus/dsl-sixty-five-oh-two/master/fancy_banner.png)

_... shut up, show me the code!_

Here's some example code utilizing all of the features of the eDSL:

```haskell
import SixtyFiveOhTwo.Instruction

accumulatorLoadNStore :: Instruction
accumulatorLoadNStore = do
    lda (Immediate 0x10)
    sta (Absolute 0x0200)
    rts (Implied)

myProgram :: Instruction
myProgram = do
    define "accumulatorLoadNStore" accumulatorLoadNStore
    call "accumulatorLoadNStore"
```

Here's a fun little snippet that adds 10 to the accumulator using Haskell Monad Magic:

```haskell
test3f2 :: Instruction
test3f2 = replicateM_ 10 (inc (Accumulator))
```

More documentation coming soon!
