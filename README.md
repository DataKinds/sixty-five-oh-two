# DSL.SixtyFiveOhTwo: A 65C02 Assembly eDSL in Haskell

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

Here's a fun little snippet that adds 10 to the accumulator using Haskell Monad Magic:tm::

```haskell
test3f2 :: Instruction
test3f2 = replicateM_ 10 (inc (Accumulator))
```

Everything that this module exposes is in [src/DSL/SixtyFiveOhTwo.hs](https://github.com/Aearnus/dsl-sixty-five-oh-two/blob/master/src/DSL/SixtyFiveOhTwo.hs). A quick browse through this file will reveal the full extent of the features of this eDSL.

## What does the language provide me?

* **Full coverage**. Everything bit of code that the 65C02 can understand is represented in this language. Everywhere `adc`  to `wai` can be used. These opcodes are represented as generic operations, each of which simply append to the bytecode that gets passed into it. Here's an example of the definition for a certain opcode:
```haskell
lda :: AddressingMode -> Instruction
lda (Immediate b) = genericOp 169 b
lda (ZeroPage b) = genericOp 165 b
lda (ZeroPageX b) = genericOp 181 b
lda (Absolute b) = genericTwoByteOp 173 b
lda (AbsoluteX b) = genericTwoByteOp 189 b
lda (AbsoluteY b) = genericTwoByteOp 185 b
lda (ZeroPageIndirect b) = genericOp 178 b
lda (IndirectX b) = genericOp 161 b
lda (IndirectY b) = genericOp 177 b
```

* **Type safety**. Every addressing mode is represented the Haskell type system, and thus issues will be caught at compile time. The `AddressingMode` ADT is used to represent a function's addressing mode, and opcodes do not take addressing modes that they do not support.
```haskell
data AddressingMode =
    Implied |
    Accumulator |
    Immediate Word8 |
    Relative Int8 | -- Signed
    ZeroPageRelative Int8 | -- Signed
    Absolute Word16 |
    AbsoluteX Word16 |
    AbsoluteY Word16 |
    ZeroPage Word8 |
    ZeroPageX Word8 |
    ZeroPageY Word8 |
    ZeroPageIndirect Word8 |
    Indirect Word16 |
    IndirectX Word8 |
    IndirectY Word8
```


* **Easy abstractions**. The `define` and `call` keywords automatically generate the code necessary to create and call subroutines.
