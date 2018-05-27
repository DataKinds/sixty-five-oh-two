{-# LANGUAGE TemplateHaskell #-}

module DSL.SixtyFiveOhTwo where

import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Control.Lens
import Data.Word
import Data.Int
import Data.Bits

data InstructionState = InstructionState {
    -- The functionTable relates functions to their byte offsets in the compiled code
    _functionTable :: M.Map String Int,
    _bytestring :: B.ByteString
} deriving Show
makeLenses ''InstructionState

emptyState :: InstructionState
emptyState = InstructionState { _functionTable = M.empty, _bytestring = B.empty }

type Instruction = State InstructionState ()

-- This function converts the instructions into a usable bytestring. It's the meat and bones of this DSL.
runInstructions :: Instruction -> B.ByteString
runInstructions ins = (execState ins emptyState) ^. bytestring

-- Remember, it's little endian
-- Addressing modes
class AddressingMode a where

data Implied = Implied
instance AddressingMode Implied where

data Accumulator = Accumulator
instance AddressingMode Accumulator where

data Immediate = Immediate Word8
instance AddressingMode Immediate where

data Relative = Relative Int8
instance AddressingMode Relative where

data ZeroPageRelative = ZeroPageRelative Int8
instance AddressingMode ZeroPageRelative where

data Absolute = Absolute Word16
instance AddressingMode Absolute where

data AbsoluteX = AbsoluteX Word16
instance AddressingMode AbsoluteX where

data AbsoluteY = AbsoluteY Word16
instance AddressingMode AbsoluteY where

data ZeroPage = ZeroPage Word8
instance AddressingMode ZeroPage where

data ZeroPageX = ZeroPageX Word8
instance AddressingMode ZeroPageX where

data ZeroPageY = ZeroPageY Word8
instance AddressingMode ZeroPageY where

data ZeroPageIndirect = ZeroPageIndirect Word8
instance AddressingMode ZeroPageIndirect where

data Indirect = Indirect Word16
instance AddressingMode Indirect where

data IndirectX = IndirectX Word8
instance AddressingMode IndirectX where

data IndirectY = IndirectY Word8
instance AddressingMode IndirectY where


splitW16 :: Word16 -> (Word8, Word8)
splitW16 w = (lo, hi) -- Little endian
    where
        hi = fromIntegral $ w `shiftR` 8
        lo = fromIntegral w

appendBytes :: [Word8] -> InstructionState -> InstructionState
appendBytes bytes insState = over bytestring (\bs -> B.append bs (B.pack bytes)) insState

appendBytesThenWord :: [Word8] -> Word16 -> InstructionState -> InstructionState
appendBytesThenWord bytes word insState = over bytestring (\bs -> B.append bs (B.pack totalBytes)) insState
    where
        (lowByte, highByte) = splitW16 word
        totalBytes = concat [bytes, [lowByte], [highByte]]

-- This function allows you to define an instruction opcode that takes no argument
genericNoByteOp :: Word8 -> Instruction
genericNoByteOp op = modify $ appendBytes [op]

-- This function allows you to define an instruction opcode that takes a one byte argument
-- This is polymorphic to support Int8 OR Word8
genericOp :: (FiniteBits a, Integral a) => Word8 -> a -> Instruction
-- fromIntegral from an IntN to a WordN does _not_ preserve value, only structure
-- Thus, this is valid code.
genericOp op arg = modify $ appendBytes [op, fromIntegral arg]

--  This function allows you to define an instruction opcode that takes a two byte argument
genericTwoByteOp :: Word8 -> Word16 -> Instruction
genericTwoByteOp op arg = modify $ appendBytesThenWord [op] arg

-- This allows you to define subroutines which can be called later using `call`.
-- NOTE: your function must end with an `rts`. This is not added implicitly to
-- be able to use this function to create branching case statements or the like.
define :: String -> Instruction -> Instruction
define name definition = do
    insState <- get
    let functionOffset = B.length $ insState ^. bytestring
    let modifyFunctionTable = \table ->
            M.insert name functionOffset table
    -- insState' is the modified state before definition compilation
    let insState' = over functionTable modifyFunctionTable insState
    -- insState'' is the modified state after definition compilation
    let insState'' = execState definition insState'
    -- The final state uses these following things:
    --   The compiled bytestring from insState''
    --   The function table from insState', WITH the additions from insState'' modified properly
    let newlyDefinedFunctions = M.difference (insState'' ^. functionTable) (insState' ^. functionTable)
    -- NOTE: because of the order of the next line, function shadowing in the DSL is impossible. the first
    -- definition is always the one that's used.
    -- The fmap is done to shift any definitions made inside this definition to their correct positions
    -- in the global scope.
    let finalFunctionTable = M.union (insState' ^. functionTable) (fmap (+ functionOffset) (insState'' ^. functionTable))
    let finalInsState = set functionTable finalFunctionTable insState''
    put finalInsState

-- This can be used to call subroutines which were previously `define`d.
call :: String -> Instruction
call name = do
    insState <- get
    let pointer = case (M.lookup name (insState ^. functionTable)) of
                    Just ptr -> ptr
                    Nothing -> error ("Couldn't find function " ++ name ++ ". Perhaps it wasn't `define`d?")
    put $ execState (jsr (Absolute $ fromIntegral pointer)) insState

-- THE FOLLOWING WAS GENERATED BY
-- https://github.com/aearnus/assemblicom
-- for the 65C02 instruction set
{-
adc :: (AddressingMode a) => a -> Instruction
adc (Immediate b) = genericOp 105 b
adc (ZeroPage b) = genericOp 101 b
adc (ZeroPageX b) = genericOp 117 b
adc (Absolute b) = genericTwoByteOp 109 b
adc (AbsoluteX b) = genericTwoByteOp 125 b
adc (AbsoluteY b) = genericTwoByteOp 121 b
adc (ZeroPageIndirect b) = genericOp 114 b
adc (IndirectX b) = genericOp 97 b
adc (IndirectY b) = genericOp 113 b

and :: (AddressingMode a) => a -> Instruction
and (Immediate b) = genericOp 41 b
and (ZeroPage b) = genericOp 37 b
and (ZeroPageX b) = genericOp 53 b
and (Absolute b) = genericTwoByteOp 45 b
and (AbsoluteX b) = genericTwoByteOp 61 b
and (AbsoluteY b) = genericTwoByteOp 57 b
and (ZeroPageIndirect b) = genericOp 50 b
and (IndirectX b) = genericOp 33 b
and (IndirectY b) = genericOp 49 b

asl :: (AddressingMode a) => a -> Instruction
asl (Accumulator) = genericNoByteOp 10
asl (ZeroPage b) = genericOp 6 b
asl (ZeroPageX b) = genericOp 22 b
asl (Absolute b) = genericTwoByteOp 14 b
asl (AbsoluteX b) = genericTwoByteOp 30 b

bbr0 :: (AddressingMode a) => a -> Instruction
bbr0 (ZeroPageRelative b) = genericOp 15 b

bbr1 :: (AddressingMode a) => a -> Instruction
bbr1 (ZeroPageRelative b) = genericOp 31 b

bbr2 :: (AddressingMode a) => a -> Instruction
bbr2 (ZeroPageRelative b) = genericOp 47 b

bbr3 :: (AddressingMode a) => a -> Instruction
bbr3 (ZeroPageRelative b) = genericOp 63 b

bbr4 :: (AddressingMode a) => a -> Instruction
bbr4 (ZeroPageRelative b) = genericOp 79 b

bbr5 :: (AddressingMode a) => a -> Instruction
bbr5 (ZeroPageRelative b) = genericOp 95 b

bbr6 :: (AddressingMode a) => a -> Instruction
bbr6 (ZeroPageRelative b) = genericOp 111 b

bbr7 :: (AddressingMode a) => a -> Instruction
bbr7 (ZeroPageRelative b) = genericOp 127 b

bbs0 :: (AddressingMode a) => a -> Instruction
bbs0 (ZeroPageRelative b) = genericOp 143 b

bbs1 :: (AddressingMode a) => a -> Instruction
bbs1 (ZeroPageRelative b) = genericOp 159 b

bbs2 :: (AddressingMode a) => a -> Instruction
bbs2 (ZeroPageRelative b) = genericOp 175 b

bbs3 :: (AddressingMode a) => a -> Instruction
bbs3 (ZeroPageRelative b) = genericOp 191 b

bbs4 :: (AddressingMode a) => a -> Instruction
bbs4 (ZeroPageRelative b) = genericOp 207 b

bbs5 :: (AddressingMode a) => a -> Instruction
bbs5 (ZeroPageRelative b) = genericOp 223 b

bbs6 :: (AddressingMode a) => a -> Instruction
bbs6 (ZeroPageRelative b) = genericOp 239 b

bbs7 :: (AddressingMode a) => a -> Instruction
bbs7 (ZeroPageRelative b) = genericOp 255 b

bcc :: (AddressingMode a) => a -> Instruction
bcc (Relative b) = genericOp 144 b

bcs :: (AddressingMode a) => a -> Instruction
bcs (Relative b) = genericOp 176 b

beq :: (AddressingMode a) => a -> Instruction
beq (Relative b) = genericOp 240 b

bit :: (AddressingMode a) => a -> Instruction
bit (Immediate b) = genericOp 137 b
bit (ZeroPage b) = genericOp 36 b
bit (ZeroPageX b) = genericOp 52 b
bit (Absolute b) = genericTwoByteOp 44 b
bit (AbsoluteX b) = genericTwoByteOp 60 b

bmi :: (AddressingMode a) => a -> Instruction
bmi (Relative b) = genericOp 48 b

bne :: (AddressingMode a) => a -> Instruction
bne (Relative b) = genericOp 208 b

bpl :: (AddressingMode a) => a -> Instruction
bpl (Relative b) = genericOp 16 b

bra :: (AddressingMode a) => a -> Instruction
bra (Relative b) = genericOp 128 b

brk :: (AddressingMode a) => a -> Instruction
brk (Implied) = genericNoByteOp 0

bvc :: (AddressingMode a) => a -> Instruction
bvc (Relative b) = genericOp 80 b

bvs :: (AddressingMode a) => a -> Instruction
bvs (Relative b) = genericOp 112 b

clc :: (AddressingMode a) => a -> Instruction
clc (Implied) = genericNoByteOp 24

cld :: (AddressingMode a) => a -> Instruction
cld (Implied) = genericNoByteOp 216

cli :: (AddressingMode a) => a -> Instruction
cli (Implied) = genericNoByteOp 88

clv :: (AddressingMode a) => a -> Instruction
clv (Implied) = genericNoByteOp 184

cmp :: (AddressingMode a) => a -> Instruction
cmp (Immediate b) = genericOp 201 b
cmp (ZeroPage b) = genericOp 197 b
cmp (ZeroPageX b) = genericOp 213 b
cmp (Absolute b) = genericTwoByteOp 205 b
cmp (AbsoluteX b) = genericTwoByteOp 221 b
cmp (AbsoluteY b) = genericTwoByteOp 217 b
cmp (ZeroPageIndirect b) = genericOp 210 b
cmp (IndirectX b) = genericOp 193 b
cmp (IndirectY b) = genericOp 209 b

cpx :: (AddressingMode a) => a -> Instruction
cpx (Immediate b) = genericOp 224 b
cpx (ZeroPage b) = genericOp 228 b
cpx (Absolute b) = genericTwoByteOp 236 b

cpy :: (AddressingMode a) => a -> Instruction
cpy (Immediate b) = genericOp 192 b
cpy (ZeroPage b) = genericOp 196 b
cpy (Absolute b) = genericTwoByteOp 204 b

dec :: (AddressingMode a) => a -> Instruction
dec (Accumulator) = genericNoByteOp 58
dec (ZeroPage b) = genericOp 198 b
dec (ZeroPageX b) = genericOp 214 b
dec (Absolute b) = genericTwoByteOp 206 b
dec (AbsoluteX b) = genericTwoByteOp 222 b

dex :: (AddressingMode a) => a -> Instruction
dex (Implied) = genericNoByteOp 202

dey :: (AddressingMode a) => a -> Instruction
dey (Implied) = genericNoByteOp 136

eor :: (AddressingMode a) => a -> Instruction
eor (Immediate b) = genericOp 73 b
eor (ZeroPage b) = genericOp 69 b
eor (ZeroPageX b) = genericOp 85 b
eor (Absolute b) = genericTwoByteOp 77 b
eor (AbsoluteX b) = genericTwoByteOp 93 b
eor (AbsoluteY b) = genericTwoByteOp 89 b
eor (ZeroPageIndirect b) = genericOp 82 b
eor (IndirectX b) = genericOp 65 b
eor (IndirectY b) = genericOp 81 b

inc :: (AddressingMode a) => a -> Instruction
inc (Accumulator) = genericNoByteOp 26
inc (ZeroPage b) = genericOp 230 b
inc (ZeroPageX b) = genericOp 246 b
inc (Absolute b) = genericTwoByteOp 238 b
inc (AbsoluteX b) = genericTwoByteOp 254 b

inx :: (AddressingMode a) => a -> Instruction
inx (Implied) = genericNoByteOp 232

iny :: (AddressingMode a) => a -> Instruction
iny (Implied) = genericNoByteOp 200

jmp :: (AddressingMode a) => a -> Instruction
jmp (Absolute b) = genericTwoByteOp 76 b
jmp (Indirect b) = genericTwoByteOp 108 b
jmp (AbsoluteX b) = genericTwoByteOp 124 b

jsr :: (AddressingMode a) => a -> Instruction
jsr (Absolute b) = genericTwoByteOp 32 b

lda :: (AddressingMode a) => a -> Instruction
lda (Immediate b) = genericOp 169 b
lda (ZeroPage b) = genericOp 165 b
lda (ZeroPageX b) = genericOp 181 b
lda (Absolute b) = genericTwoByteOp 173 b
lda (AbsoluteX b) = genericTwoByteOp 189 b
lda (AbsoluteY b) = genericTwoByteOp 185 b
lda (ZeroPageIndirect b) = genericOp 178 b
lda (IndirectX b) = genericOp 161 b
lda (IndirectY b) = genericOp 177 b

ldx :: (AddressingMode a) => a -> Instruction
ldx (Immediate b) = genericOp 162 b
ldx (ZeroPage b) = genericOp 166 b
ldx (ZeroPageY b) = genericOp 182 b
ldx (Absolute b) = genericTwoByteOp 174 b
ldx (AbsoluteY b) = genericTwoByteOp 190 b

ldy :: (AddressingMode a) => a -> Instruction
ldy (Immediate b) = genericOp 160 b
ldy (ZeroPage b) = genericOp 164 b
ldy (ZeroPageX b) = genericOp 180 b
ldy (Absolute b) = genericTwoByteOp 172 b
ldy (AbsoluteX b) = genericTwoByteOp 188 b

lsr :: (AddressingMode a) => a -> Instruction
lsr (Accumulator) = genericNoByteOp 74
lsr (ZeroPage b) = genericOp 70 b
lsr (ZeroPageX b) = genericOp 86 b
lsr (Absolute b) = genericTwoByteOp 78 b
lsr (AbsoluteX b) = genericTwoByteOp 94 b

nop :: (AddressingMode a) => a -> Instruction
nop (Implied) = genericNoByteOp 234

ora :: (AddressingMode a) => a -> Instruction
ora (Immediate b) = genericOp 9 b
ora (ZeroPage b) = genericOp 5 b
ora (ZeroPageX b) = genericOp 21 b
ora (Absolute b) = genericTwoByteOp 13 b
ora (AbsoluteX b) = genericTwoByteOp 29 b
ora (AbsoluteY b) = genericTwoByteOp 25 b
ora (ZeroPageIndirect b) = genericOp 18 b
ora (IndirectX b) = genericOp 1 b
ora (IndirectY b) = genericOp 17 b

pha :: (AddressingMode a) => a -> Instruction
pha (Implied) = genericNoByteOp 72

php :: (AddressingMode a) => a -> Instruction
php (Implied) = genericNoByteOp 8

phx :: (AddressingMode a) => a -> Instruction
phx (Implied) = genericNoByteOp 218

phy :: (AddressingMode a) => a -> Instruction
phy (Implied) = genericNoByteOp 90

pla :: (AddressingMode a) => a -> Instruction
pla (Implied) = genericNoByteOp 104

plp :: (AddressingMode a) => a -> Instruction
plp (Implied) = genericNoByteOp 40

plx :: (AddressingMode a) => a -> Instruction
plx (Implied) = genericNoByteOp 250

ply :: (AddressingMode a) => a -> Instruction
ply (Implied) = genericNoByteOp 122

rmb0 :: (AddressingMode a) => a -> Instruction
rmb0 (ZeroPage b) = genericOp 7 b

rmb1 :: (AddressingMode a) => a -> Instruction
rmb1 (ZeroPage b) = genericOp 23 b

rmb2 :: (AddressingMode a) => a -> Instruction
rmb2 (ZeroPage b) = genericOp 39 b

rmb3 :: (AddressingMode a) => a -> Instruction
rmb3 (ZeroPage b) = genericOp 55 b

rmb4 :: (AddressingMode a) => a -> Instruction
rmb4 (ZeroPage b) = genericOp 71 b

rmb5 :: (AddressingMode a) => a -> Instruction
rmb5 (ZeroPage b) = genericOp 87 b

rmb6 :: (AddressingMode a) => a -> Instruction
rmb6 (ZeroPage b) = genericOp 103 b

rmb7 :: (AddressingMode a) => a -> Instruction
rmb7 (ZeroPage b) = genericOp 119 b

rol :: (AddressingMode a) => a -> Instruction
rol (Accumulator) = genericNoByteOp 42
rol (ZeroPage b) = genericOp 38 b
rol (ZeroPageX b) = genericOp 54 b
rol (Absolute b) = genericTwoByteOp 46 b
rol (AbsoluteX b) = genericTwoByteOp 62 b

ror :: (AddressingMode a) => a -> Instruction
ror (Accumulator) = genericNoByteOp 106
ror (ZeroPage b) = genericOp 102 b
ror (ZeroPageX b) = genericOp 118 b
ror (Absolute b) = genericTwoByteOp 110 b
ror (AbsoluteX b) = genericTwoByteOp 126 b

rti :: (AddressingMode a) => a -> Instruction
rti (Implied) = genericNoByteOp 64

rts :: (AddressingMode a) => a -> Instruction
rts (Implied) = genericNoByteOp 96

sbc :: (AddressingMode a) => a -> Instruction
sbc (Immediate b) = genericOp 233 b
sbc (ZeroPage b) = genericOp 229 b
sbc (ZeroPageX b) = genericOp 245 b
sbc (Absolute b) = genericTwoByteOp 237 b
sbc (AbsoluteX b) = genericTwoByteOp 253 b
sbc (AbsoluteY b) = genericTwoByteOp 249 b
sbc (ZeroPageIndirect b) = genericOp 242 b
sbc (IndirectX b) = genericOp 225 b
sbc (IndirectY b) = genericOp 241 b

sec :: (AddressingMode a) => a -> Instruction
sec (Implied) = genericNoByteOp 56

sed :: (AddressingMode a) => a -> Instruction
sed (Implied) = genericNoByteOp 248

sei :: (AddressingMode a) => a -> Instruction
sei (Implied) = genericNoByteOp 120

smb0 :: (AddressingMode a) => a -> Instruction
smb0 (ZeroPage b) = genericOp 135 b

smb1 :: (AddressingMode a) => a -> Instruction
smb1 (ZeroPage b) = genericOp 151 b

smb2 :: (AddressingMode a) => a -> Instruction
smb2 (ZeroPage b) = genericOp 167 b

smb3 :: (AddressingMode a) => a -> Instruction
smb3 (ZeroPage b) = genericOp 183 b

smb4 :: (AddressingMode a) => a -> Instruction
smb4 (ZeroPage b) = genericOp 199 b

smb5 :: (AddressingMode a) => a -> Instruction
smb5 (ZeroPage b) = genericOp 215 b

smb6 :: (AddressingMode a) => a -> Instruction
smb6 (ZeroPage b) = genericOp 231 b

smb7 :: (AddressingMode a) => a -> Instruction
smb7 (ZeroPage b) = genericOp 247 b

sta :: (AddressingMode a) => a -> Instruction
sta (ZeroPage b) = genericOp 133 b
sta (ZeroPageX b) = genericOp 149 b
sta (Absolute b) = genericTwoByteOp 141 b
sta (AbsoluteX b) = genericTwoByteOp 157 b
sta (AbsoluteY b) = genericTwoByteOp 153 b
sta (ZeroPageIndirect b) = genericOp 146 b
sta (IndirectX b) = genericOp 129 b
sta (IndirectY b) = genericOp 145 b

stp :: (AddressingMode a) => a -> Instruction
stp (Implied) = genericNoByteOp 219

stx :: (AddressingMode a) => a -> Instruction
stx (ZeroPage b) = genericOp 134 b
stx (ZeroPageY b) = genericOp 150 b
stx (Absolute b) = genericTwoByteOp 142 b

sty :: (AddressingMode a) => a -> Instruction
sty (ZeroPage b) = genericOp 132 b
sty (ZeroPageX b) = genericOp 148 b
sty (Absolute b) = genericTwoByteOp 140 b

stz :: (AddressingMode a) => a -> Instruction
stz (ZeroPage b) = genericOp 100 b
stz (ZeroPageX b) = genericOp 116 b
stz (Absolute b) = genericTwoByteOp 156 b
stz (AbsoluteX b) = genericTwoByteOp 158 b

tax :: (AddressingMode a) => a -> Instruction
tax (Implied) = genericNoByteOp 170

tay :: (AddressingMode a) => a -> Instruction
tay (Implied) = genericNoByteOp 168

trb :: (AddressingMode a) => a -> Instruction
trb (ZeroPage b) = genericOp 20 b
trb (Absolute b) = genericTwoByteOp 28 b

tsb :: (AddressingMode a) => a -> Instruction
tsb (ZeroPage b) = genericOp 4 b
tsb (Absolute b) = genericTwoByteOp 12 b

tsx :: (AddressingMode a) => a -> Instruction
tsx (Implied) = genericNoByteOp 186

txa :: (AddressingMode a) => a -> Instruction
txa (Implied) = genericNoByteOp 138

txs :: (AddressingMode a) => a -> Instruction
txs (Implied) = genericNoByteOp 154

tya :: (AddressingMode a) => a -> Instruction
tya (Implied) = genericNoByteOp 152

wai :: (AddressingMode a) => a -> Instruction
wai (Implied) = genericNoByteOp 203
-}
adc :: (AddressingMode a) => a -> Instruction
adc mode = case mode of
	(Immediate b) -> genericOp 105 b
	(ZeroPage b) -> genericOp 101 b
	(ZeroPageX b) -> genericOp 117 b
	(Absolute b) -> genericTwoByteOp 109 b
	(AbsoluteX b) -> genericTwoByteOp 125 b
	(AbsoluteY b) -> genericTwoByteOp 121 b
	(ZeroPageIndirect b) -> genericOp 114 b
	(IndirectX b) -> genericOp 97 b
	(IndirectY b) -> genericOp 113 b

and :: (AddressingMode a) => a -> Instruction
and mode = case mode of
	(Immediate b) -> genericOp 41 b
	(ZeroPage b) -> genericOp 37 b
	(ZeroPageX b) -> genericOp 53 b
	(Absolute b) -> genericTwoByteOp 45 b
	(AbsoluteX b) -> genericTwoByteOp 61 b
	(AbsoluteY b) -> genericTwoByteOp 57 b
	(ZeroPageIndirect b) -> genericOp 50 b
	(IndirectX b) -> genericOp 33 b
	(IndirectY b) -> genericOp 49 b

asl :: (AddressingMode a) => a -> Instruction
asl mode = case mode of
	Accumulator -> genericNoByteOp 10
	(ZeroPage b) -> genericOp 6 b
	(ZeroPageX b) -> genericOp 22 b
	(Absolute b) -> genericTwoByteOp 14 b
	(AbsoluteX b) -> genericTwoByteOp 30 b

bbr0 :: (AddressingMode a) => a -> Instruction
bbr0 mode = case mode of
	(ZeroPageRelative b) -> genericOp 15 b

bbr1 :: (AddressingMode a) => a -> Instruction
bbr1 mode = case mode of
	(ZeroPageRelative b) -> genericOp 31 b

bbr2 :: (AddressingMode a) => a -> Instruction
bbr2 mode = case mode of
	(ZeroPageRelative b) -> genericOp 47 b

bbr3 :: (AddressingMode a) => a -> Instruction
bbr3 mode = case mode of
	(ZeroPageRelative b) -> genericOp 63 b

bbr4 :: (AddressingMode a) => a -> Instruction
bbr4 mode = case mode of
	(ZeroPageRelative b) -> genericOp 79 b

bbr5 :: (AddressingMode a) => a -> Instruction
bbr5 mode = case mode of
	(ZeroPageRelative b) -> genericOp 95 b

bbr6 :: (AddressingMode a) => a -> Instruction
bbr6 mode = case mode of
	(ZeroPageRelative b) -> genericOp 111 b

bbr7 :: (AddressingMode a) => a -> Instruction
bbr7 mode = case mode of
	(ZeroPageRelative b) -> genericOp 127 b

bbs0 :: (AddressingMode a) => a -> Instruction
bbs0 mode = case mode of
	(ZeroPageRelative b) -> genericOp 143 b

bbs1 :: (AddressingMode a) => a -> Instruction
bbs1 mode = case mode of
	(ZeroPageRelative b) -> genericOp 159 b

bbs2 :: (AddressingMode a) => a -> Instruction
bbs2 mode = case mode of
	(ZeroPageRelative b) -> genericOp 175 b

bbs3 :: (AddressingMode a) => a -> Instruction
bbs3 mode = case mode of
	(ZeroPageRelative b) -> genericOp 191 b

bbs4 :: (AddressingMode a) => a -> Instruction
bbs4 mode = case mode of
	(ZeroPageRelative b) -> genericOp 207 b

bbs5 :: (AddressingMode a) => a -> Instruction
bbs5 mode = case mode of
	(ZeroPageRelative b) -> genericOp 223 b

bbs6 :: (AddressingMode a) => a -> Instruction
bbs6 mode = case mode of
	(ZeroPageRelative b) -> genericOp 239 b

bbs7 :: (AddressingMode a) => a -> Instruction
bbs7 mode = case mode of
	(ZeroPageRelative b) -> genericOp 255 b

bcc :: (AddressingMode a) => a -> Instruction
bcc mode = case mode of
	(Relative b) -> genericOp 144 b

bcs :: (AddressingMode a) => a -> Instruction
bcs mode = case mode of
	(Relative b) -> genericOp 176 b

beq :: (AddressingMode a) => a -> Instruction
beq mode = case mode of
	(Relative b) -> genericOp 240 b

bit :: (AddressingMode a) => a -> Instruction
bit mode = case mode of
	(Immediate b) -> genericOp 137 b
	(ZeroPage b) -> genericOp 36 b
	(ZeroPageX b) -> genericOp 52 b
	(Absolute b) -> genericTwoByteOp 44 b
	(AbsoluteX b) -> genericTwoByteOp 60 b

bmi :: (AddressingMode a) => a -> Instruction
bmi mode = case mode of
	(Relative b) -> genericOp 48 b

bne :: (AddressingMode a) => a -> Instruction
bne mode = case mode of
	(Relative b) -> genericOp 208 b

bpl :: (AddressingMode a) => a -> Instruction
bpl mode = case mode of
	(Relative b) -> genericOp 16 b

bra :: (AddressingMode a) => a -> Instruction
bra mode = case mode of
	(Relative b) -> genericOp 128 b

brk :: (AddressingMode a) => a -> Instruction
brk mode = case mode of
	Implied -> genericNoByteOp 0

bvc :: (AddressingMode a) => a -> Instruction
bvc mode = case mode of
	(Relative b) -> genericOp 80 b

bvs :: (AddressingMode a) => a -> Instruction
bvs mode = case mode of
	(Relative b) -> genericOp 112 b

clc :: (AddressingMode a) => a -> Instruction
clc mode = case mode of
	Implied -> genericNoByteOp 24

cld :: (AddressingMode a) => a -> Instruction
cld mode = case mode of
	Implied -> genericNoByteOp 216

cli :: (AddressingMode a) => a -> Instruction
cli mode = case mode of
	Implied -> genericNoByteOp 88

clv :: (AddressingMode a) => a -> Instruction
clv mode = case mode of
	Implied -> genericNoByteOp 184

cmp :: (AddressingMode a) => a -> Instruction
cmp mode = case mode of
	(Immediate b) -> genericOp 201 b
	(ZeroPage b) -> genericOp 197 b
	(ZeroPageX b) -> genericOp 213 b
	(Absolute b) -> genericTwoByteOp 205 b
	(AbsoluteX b) -> genericTwoByteOp 221 b
	(AbsoluteY b) -> genericTwoByteOp 217 b
	(ZeroPageIndirect b) -> genericOp 210 b
	(IndirectX b) -> genericOp 193 b
	(IndirectY b) -> genericOp 209 b

cpx :: (AddressingMode a) => a -> Instruction
cpx mode = case mode of
	(Immediate b) -> genericOp 224 b
	(ZeroPage b) -> genericOp 228 b
	(Absolute b) -> genericTwoByteOp 236 b

cpy :: (AddressingMode a) => a -> Instruction
cpy mode = case mode of
	(Immediate b) -> genericOp 192 b
	(ZeroPage b) -> genericOp 196 b
	(Absolute b) -> genericTwoByteOp 204 b

dec :: (AddressingMode a) => a -> Instruction
dec mode = case mode of
	Accumulator -> genericNoByteOp 58
	(ZeroPage b) -> genericOp 198 b
	(ZeroPageX b) -> genericOp 214 b
	(Absolute b) -> genericTwoByteOp 206 b
	(AbsoluteX b) -> genericTwoByteOp 222 b

dex :: (AddressingMode a) => a -> Instruction
dex mode = case mode of
	Implied -> genericNoByteOp 202

dey :: (AddressingMode a) => a -> Instruction
dey mode = case mode of
	Implied -> genericNoByteOp 136

eor :: (AddressingMode a) => a -> Instruction
eor mode = case mode of
	(Immediate b) -> genericOp 73 b
	(ZeroPage b) -> genericOp 69 b
	(ZeroPageX b) -> genericOp 85 b
	(Absolute b) -> genericTwoByteOp 77 b
	(AbsoluteX b) -> genericTwoByteOp 93 b
	(AbsoluteY b) -> genericTwoByteOp 89 b
	(ZeroPageIndirect b) -> genericOp 82 b
	(IndirectX b) -> genericOp 65 b
	(IndirectY b) -> genericOp 81 b

inc :: (AddressingMode a) => a -> Instruction
inc mode = case mode of
	Accumulator -> genericNoByteOp 26
	(ZeroPage b) -> genericOp 230 b
	(ZeroPageX b) -> genericOp 246 b
	(Absolute b) -> genericTwoByteOp 238 b
	(AbsoluteX b) -> genericTwoByteOp 254 b

inx :: (AddressingMode a) => a -> Instruction
inx mode = case mode of
	Implied -> genericNoByteOp 232

iny :: (AddressingMode a) => a -> Instruction
iny mode = case mode of
	Implied -> genericNoByteOp 200

jmp :: (AddressingMode a) => a -> Instruction
jmp mode = case mode of
	(Absolute b) -> genericTwoByteOp 76 b
	(Indirect b) -> genericTwoByteOp 108 b
	(AbsoluteX b) -> genericTwoByteOp 124 b

jsr :: (AddressingMode a) => a -> Instruction
jsr mode = case mode of
	(Absolute b) -> genericTwoByteOp 32 b

lda :: (AddressingMode a) => a -> Instruction
lda mode = case mode of
	(Immediate b) -> genericOp 169 b
	(ZeroPage b) -> genericOp 165 b
	(ZeroPageX b) -> genericOp 181 b
	(Absolute b) -> genericTwoByteOp 173 b
	(AbsoluteX b) -> genericTwoByteOp 189 b
	(AbsoluteY b) -> genericTwoByteOp 185 b
	(ZeroPageIndirect b) -> genericOp 178 b
	(IndirectX b) -> genericOp 161 b
	(IndirectY b) -> genericOp 177 b

ldx :: (AddressingMode a) => a -> Instruction
ldx mode = case mode of
	(Immediate b) -> genericOp 162 b
	(ZeroPage b) -> genericOp 166 b
	(ZeroPageY b) -> genericOp 182 b
	(Absolute b) -> genericTwoByteOp 174 b
	(AbsoluteY b) -> genericTwoByteOp 190 b

ldy :: (AddressingMode a) => a -> Instruction
ldy mode = case mode of
	(Immediate b) -> genericOp 160 b
	(ZeroPage b) -> genericOp 164 b
	(ZeroPageX b) -> genericOp 180 b
	(Absolute b) -> genericTwoByteOp 172 b
	(AbsoluteX b) -> genericTwoByteOp 188 b

lsr :: (AddressingMode a) => a -> Instruction
lsr mode = case mode of
	Accumulator -> genericNoByteOp 74
	(ZeroPage b) -> genericOp 70 b
	(ZeroPageX b) -> genericOp 86 b
	(Absolute b) -> genericTwoByteOp 78 b
	(AbsoluteX b) -> genericTwoByteOp 94 b

nop :: (AddressingMode a) => a -> Instruction
nop mode = case mode of
	Implied -> genericNoByteOp 234

ora :: (AddressingMode a) => a -> Instruction
ora mode = case mode of
	(Immediate b) -> genericOp 9 b
	(ZeroPage b) -> genericOp 5 b
	(ZeroPageX b) -> genericOp 21 b
	(Absolute b) -> genericTwoByteOp 13 b
	(AbsoluteX b) -> genericTwoByteOp 29 b
	(AbsoluteY b) -> genericTwoByteOp 25 b
	(ZeroPageIndirect b) -> genericOp 18 b
	(IndirectX b) -> genericOp 1 b
	(IndirectY b) -> genericOp 17 b

pha :: (AddressingMode a) => a -> Instruction
pha mode = case mode of
	Implied -> genericNoByteOp 72

php :: (AddressingMode a) => a -> Instruction
php mode = case mode of
	Implied -> genericNoByteOp 8

phx :: (AddressingMode a) => a -> Instruction
phx mode = case mode of
	Implied -> genericNoByteOp 218

phy :: (AddressingMode a) => a -> Instruction
phy mode = case mode of
	Implied -> genericNoByteOp 90

pla :: (AddressingMode a) => a -> Instruction
pla mode = case mode of
	Implied -> genericNoByteOp 104

plp :: (AddressingMode a) => a -> Instruction
plp mode = case mode of
	Implied -> genericNoByteOp 40

plx :: (AddressingMode a) => a -> Instruction
plx mode = case mode of
	Implied -> genericNoByteOp 250

ply :: (AddressingMode a) => a -> Instruction
ply mode = case mode of
	Implied -> genericNoByteOp 122

rmb0 :: (AddressingMode a) => a -> Instruction
rmb0 mode = case mode of
	(ZeroPage b) -> genericOp 7 b

rmb1 :: (AddressingMode a) => a -> Instruction
rmb1 mode = case mode of
	(ZeroPage b) -> genericOp 23 b

rmb2 :: (AddressingMode a) => a -> Instruction
rmb2 mode = case mode of
	(ZeroPage b) -> genericOp 39 b

rmb3 :: (AddressingMode a) => a -> Instruction
rmb3 mode = case mode of
	(ZeroPage b) -> genericOp 55 b

rmb4 :: (AddressingMode a) => a -> Instruction
rmb4 mode = case mode of
	(ZeroPage b) -> genericOp 71 b

rmb5 :: (AddressingMode a) => a -> Instruction
rmb5 mode = case mode of
	(ZeroPage b) -> genericOp 87 b

rmb6 :: (AddressingMode a) => a -> Instruction
rmb6 mode = case mode of
	(ZeroPage b) -> genericOp 103 b

rmb7 :: (AddressingMode a) => a -> Instruction
rmb7 mode = case mode of
	(ZeroPage b) -> genericOp 119 b

rol :: (AddressingMode a) => a -> Instruction
rol mode = case mode of
	Accumulator -> genericNoByteOp 42
	(ZeroPage b) -> genericOp 38 b
	(ZeroPageX b) -> genericOp 54 b
	(Absolute b) -> genericTwoByteOp 46 b
	(AbsoluteX b) -> genericTwoByteOp 62 b

ror :: (AddressingMode a) => a -> Instruction
ror mode = case mode of
	Accumulator -> genericNoByteOp 106
	(ZeroPage b) -> genericOp 102 b
	(ZeroPageX b) -> genericOp 118 b
	(Absolute b) -> genericTwoByteOp 110 b
	(AbsoluteX b) -> genericTwoByteOp 126 b

rti :: (AddressingMode a) => a -> Instruction
rti mode = case mode of
	Implied -> genericNoByteOp 64

rts :: (AddressingMode a) => a -> Instruction
rts mode = case mode of
	Implied -> genericNoByteOp 96

sbc :: (AddressingMode a) => a -> Instruction
sbc mode = case mode of
	(Immediate b) -> genericOp 233 b
	(ZeroPage b) -> genericOp 229 b
	(ZeroPageX b) -> genericOp 245 b
	(Absolute b) -> genericTwoByteOp 237 b
	(AbsoluteX b) -> genericTwoByteOp 253 b
	(AbsoluteY b) -> genericTwoByteOp 249 b
	(ZeroPageIndirect b) -> genericOp 242 b
	(IndirectX b) -> genericOp 225 b
	(IndirectY b) -> genericOp 241 b

sec :: (AddressingMode a) => a -> Instruction
sec mode = case mode of
	Implied -> genericNoByteOp 56

sed :: (AddressingMode a) => a -> Instruction
sed mode = case mode of
	Implied -> genericNoByteOp 248

sei :: (AddressingMode a) => a -> Instruction
sei mode = case mode of
	Implied -> genericNoByteOp 120

smb0 :: (AddressingMode a) => a -> Instruction
smb0 mode = case mode of
	(ZeroPage b) -> genericOp 135 b

smb1 :: (AddressingMode a) => a -> Instruction
smb1 mode = case mode of
	(ZeroPage b) -> genericOp 151 b

smb2 :: (AddressingMode a) => a -> Instruction
smb2 mode = case mode of
	(ZeroPage b) -> genericOp 167 b

smb3 :: (AddressingMode a) => a -> Instruction
smb3 mode = case mode of
	(ZeroPage b) -> genericOp 183 b

smb4 :: (AddressingMode a) => a -> Instruction
smb4 mode = case mode of
	(ZeroPage b) -> genericOp 199 b

smb5 :: (AddressingMode a) => a -> Instruction
smb5 mode = case mode of
	(ZeroPage b) -> genericOp 215 b

smb6 :: (AddressingMode a) => a -> Instruction
smb6 mode = case mode of
	(ZeroPage b) -> genericOp 231 b

smb7 :: (AddressingMode a) => a -> Instruction
smb7 mode = case mode of
	(ZeroPage b) -> genericOp 247 b

sta :: (AddressingMode a) => a -> Instruction
sta mode = case mode of
	(ZeroPage b) -> genericOp 133 b
	(ZeroPageX b) -> genericOp 149 b
	(Absolute b) -> genericTwoByteOp 141 b
	(AbsoluteX b) -> genericTwoByteOp 157 b
	(AbsoluteY b) -> genericTwoByteOp 153 b
	(ZeroPageIndirect b) -> genericOp 146 b
	(IndirectX b) -> genericOp 129 b
	(IndirectY b) -> genericOp 145 b

stp :: (AddressingMode a) => a -> Instruction
stp mode = case mode of
	Implied -> genericNoByteOp 219

stx :: (AddressingMode a) => a -> Instruction
stx mode = case mode of
	(ZeroPage b) -> genericOp 134 b
	(ZeroPageY b) -> genericOp 150 b
	(Absolute b) -> genericTwoByteOp 142 b

sty :: (AddressingMode a) => a -> Instruction
sty mode = case mode of
	(ZeroPage b) -> genericOp 132 b
	(ZeroPageX b) -> genericOp 148 b
	(Absolute b) -> genericTwoByteOp 140 b

stz :: (AddressingMode a) => a -> Instruction
stz mode = case mode of
	(ZeroPage b) -> genericOp 100 b
	(ZeroPageX b) -> genericOp 116 b
	(Absolute b) -> genericTwoByteOp 156 b
	(AbsoluteX b) -> genericTwoByteOp 158 b

tax :: (AddressingMode a) => a -> Instruction
tax mode = case mode of
	Implied -> genericNoByteOp 170

tay :: (AddressingMode a) => a -> Instruction
tay mode = case mode of
	Implied -> genericNoByteOp 168

trb :: (AddressingMode a) => a -> Instruction
trb mode = case mode of
	(ZeroPage b) -> genericOp 20 b
	(Absolute b) -> genericTwoByteOp 28 b

tsb :: (AddressingMode a) => a -> Instruction
tsb mode = case mode of
	(ZeroPage b) -> genericOp 4 b
	(Absolute b) -> genericTwoByteOp 12 b

tsx :: (AddressingMode a) => a -> Instruction
tsx mode = case mode of
	Implied -> genericNoByteOp 186

txa :: (AddressingMode a) => a -> Instruction
txa mode = case mode of
	Implied -> genericNoByteOp 138

txs :: (AddressingMode a) => a -> Instruction
txs mode = case mode of
	Implied -> genericNoByteOp 154

tya :: (AddressingMode a) => a -> Instruction
tya mode = case mode of
	Implied -> genericNoByteOp 152

wai :: (AddressingMode a) => a -> Instruction
wai mode = case mode of
	Implied -> genericNoByteOp 203
