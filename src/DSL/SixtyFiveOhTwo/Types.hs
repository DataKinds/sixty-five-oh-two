{-# LANGUAGE TypeFamilies, TypeOperators, GADTs, DataKinds, KindSignatures, PolyKinds #-}

module DSL.SixtyFiveOhTwo.Types where

import Data.Word
import Data.Int
import Data.Bits

-- Remember, it's little endian
data AddressingKind =
    ImpliedKind |
    AccumulatorKind |
    ImmediateKind |
    RelativeKind |
    ZeroPageRelativeKind |
    AbsoluteKind |
    AbsoluteXKind |
    AbsoluteYKind |
    ZeroPageKind |
    ZeroPageXKind |
    ZeroPageYKind |
    ZeroPageIndirectKind |
    IndirectKind |
    IndirectXKind |
    IndirectYKind

data AddressingMode (k :: AddressingKind) where
    Implied :: AddressingMode 'ImpliedKind
    Accumulator :: AddressingMode 'AccumulatorKind
    Immediate :: Word8 -> AddressingMode 'ImmediateKind
    Relative :: Int8 -> AddressingMode 'RelativeKind -- Signed
    ZeroPageRelative :: Int8 -> AddressingMode 'ZeroPageRelativeKind -- Signed
    Absolute :: Word16 -> AddressingMode 'AbsoluteKind
    AbsoluteX :: Word16 -> AddressingMode 'AbsoluteXKind
    AbsoluteY :: Word16 -> AddressingMode 'AbsoluteYKind
    ZeroPage :: Word8 -> AddressingMode 'ZeroPageKind
    ZeroPageX :: Word8 -> AddressingMode 'ZeroPageXKind
    ZeroPageY :: Word8 -> AddressingMode 'ZeroPageYKind
    ZeroPageIndirect :: Word8 -> AddressingMode 'ZeroPageIndirectKind
    Indirect :: Word16 -> AddressingMode 'IndirectKind
    IndirectX :: Word8 -> AddressingMode 'IndirectXKind
    IndirectY :: Word8 -> AddressingMode 'IndirectYKind

type family IsElem (e :: k) (es :: [k]) where
    IsElem e '[] = 'False
    IsElem e (e ': es) = 'True
    IsElem e (x ': es) = IsElem e es
