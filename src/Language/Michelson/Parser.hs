{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Michelson.Parser where

import qualified Data.ByteString                  as B
import qualified Data.ByteString.Base16           as B16
import           Data.Char                        as Char
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L

import qualified Language.Michelson.Macro         as Macro
import qualified Language.Michelson.Types         as M

import           Data.Maybe
import           Data.Natural
import           Data.Void                        (Void)

import           Control.Applicative.Permutations

type Parser = Parsec Void T.Text

-- top-level parsers
contract :: Parser M.Contract
contract = do
  mSpace
  (p,s,c) <- runPermutation $
              (,,) <$> toPermutation parameter
                   <*> toPermutation storage
                   <*> toPermutation code
  return $ M.Contract p s c

parameter = do symbol "parameter"; type_ <* semicolon
storage   = do symbol "storage"; type_ <* semicolon
code      = do symbol "code"; ops <* optional semicolon

-- Lexing
lexeme = L.lexeme mSpace
mSpace = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

symbol = L.symbol mSpace
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
semicolon = symbol ";"

{- Data Parsers -}
data_ :: Parser M.Data
data_ = lexeme $ dataInner <|> parens dataInner
  where
    dataInner = try (M.Int <$> intLiteral)
          <|> try (M.String <$> stringLiteral)
          <|> try (M.Bytes <$> bytesLiteral)
          <|> do symbol "Unit"; return M.Unit
          <|> do symbol "True"; return M.True
          <|> do symbol "False"; return M.False
          <|> do symbol "Pair"; a <- data_; M.Pair a <$> data_
          <|> do symbol "Left"; M.Left <$> data_
          <|> do symbol "Right"; M.Right <$> data_
          <|> do symbol "Some"; M.Some <$> data_
          <|> do symbol "None"; return M.None
          <|> try (M.Seq <$> listData)
          <|> try (M.Map <$> mapData)
          <|> M.DataOps <$> ops

listData = braces $ sepEndBy data_ semicolon
eltData = do symbol "Elt"; key <- data_; M.Elt key <$> data_
mapData = braces $ sepEndBy eltData semicolon

intLiteral = L.signed (return ()) L.decimal

bytesLiteral = do
  symbol "0x"
  hexdigits <- takeWhile1P Nothing Char.isHexDigit
  let (bytes, remain) = B16.decode $ encodeUtf8 hexdigits
  if remain == ""
  then return bytes
  else error "odd number bytes" -- TODO: better errors

-- this parses more escape sequences than are in the michelson spec
-- should investigate which sequences this matters for, e.g. \xa == \n
stringLiteral :: Parser T.Text
stringLiteral = T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

{-
-- could do something explicit based on this
strEscape :: Parser T.Text
strEscape = char '\\' >> esc
  where
    esc = (char 'n' >> return "\n")
      <|> (char 't' >> return "\t")
      <|> (char 'b' >> return "\b")
      <|> (char '\\' >> return "\\")
      <|> (char '"' >> return "\"")
-}

{- Permutation Parsers -}
class Default a where def :: a

instance Default (Maybe a)                       where def = Nothing
instance Default [a]                             where def = []
instance (Default a, Default b) => Default (a,b) where def = (def, def)
instance Default a => Default (Parser a)         where def = pure def

permute2Def :: (Default a, Default b) => Parser a -> Parser b -> Parser (a,b)
permute2Def a b = runPermutation $
  (,) <$> toPermutationWithDefault def a
      <*> toPermutationWithDefault def b

permute3Def :: (Default a, Default b, Default c) =>
                Parser a -> Parser b -> Parser c -> Parser (a,b,c)
permute3Def a b c = runPermutation $
  (,,) <$> toPermutationWithDefault def a
       <*> toPermutationWithDefault def b
       <*> toPermutationWithDefault def c

-- General T/V/F Annotation parser
note :: T.Text -> Parser T.Text
note c = lexeme $ string c >> (note' <|> emptyNote)
  where
    emptyNote = pure ""
    note' = do
      a <- string "@"
           <|> string "%"
           <|> string "%%"
           <|> T.singleton <$> satisfy (\ x -> isAlpha x && isAscii x)
      let validChar x =
            isAscii x && (isAlphaNum x || x == '\\' || x == '.' || x == '_')
      b <- takeWhileP Nothing validChar
      return $ T.append a b

noteT :: Parser M.TypeNote
noteT = Just <$> note ":"

noteV :: Parser M.VarNote
noteV = Just <$> note "@"

noteF :: Parser M.FieldNote
noteF = Just <$> note "%"

noteF2 :: Parser (M.FieldNote, M.FieldNote)
noteF2 = do a <- noteF; b <- noteF; return (a, b)

parseDef :: Default a => Parser a -> Parser a
parseDef a = try a <|> pure def

noteTDef = parseDef noteT
noteVDef = parseDef noteV
noteFDef = parseDef noteF

notesTVF :: Parser (M.TypeNote, M.VarNote, M.FieldNote)
notesTVF = permute3Def noteT noteV noteF

notesTVF2 :: Parser (M.TypeNote, M.VarNote, (M.FieldNote, M.FieldNote))
notesTVF2 = permute3Def noteT noteV noteF2

notesTV :: Parser (M.TypeNote, M.VarNote)
notesTV = permute2Def noteT noteV

notesVF :: Parser (M.VarNote, M.FieldNote)
notesVF  = permute2Def noteV noteF

{- Type Parsers -}
--field :: Parser M.Type
--field = try (typeInner noteFDef) <|> parens (typeInner noteFDef)

field :: Parser (M.FieldNote, M.Type)
field = lexeme (fi <|> parens fi)
  where
    fi = typeInner noteF

type_ :: Parser M.Type
type_ = lexeme (ti <|> parens ti)
  where
    ti = snd <$> typeInner (pure Nothing)

typeInner :: Parser M.FieldNote -> Parser (M.FieldNote, M.Type)
typeInner fp = lexeme $
      do ct <- ct; (f,t) <- ft; return (f, M.Type (M.T_comparable ct) t)
  <|> do symbol "key"; (f,t) <- ft; return (f, M.Type M.T_key t)
  <|> do symbol "unit"; (f,t) <- ft; return (f, M.Type M.T_unit t)
  <|> do symbol "signature"; (f, t) <- ft; return (f, M.Type M.T_signature t)
  <|> do symbol "option"; (f, t) <- ft; (fa, a) <- field;
         return (f, M.Type (M.T_option fa a) t)
  <|> do symbol "list"; (f, t) <- ft; a <- type_;
         return (f, M.Type (M.T_list a) t)
  <|> do symbol "set"; (f, t) <- ft; a <- comparable;
         return (f, M.Type (M.T_set a) t)
  <|> do symbol "operation"; (f, t) <- ft; return (f, M.Type M.T_operation t)
  -- <|> (do symbol "address"; (f, t) <- ft; return (f, M.Type M.T_address t)
  <|> do symbol "contract"; (f, t) <- ft; a <- type_;
         return (f, M.Type (M.T_contract a) t)
  <|> do symbol "pair"; (f, t) <- ft; (l, a) <- field; (r, b) <- field;
         return (f, M.Type (M.T_pair l r a b) t)
  <|> do symbol "or"; (f, t) <- ft; (l, a) <- field; (r, b) <- field;
         return (f, M.Type (M.T_or l r a b) t)
  <|> do symbol "lambda"; (f, t) <- ft; a <- type_; b <- type_;
         return (f, M.Type (M.T_lambda a b) t)
  <|> do symbol "map"; (f, t) <- ft; a <- comparable; b <- type_;
         return (f, M.Type (M.T_map a b) t)
  <|> do symbol "big_map"; (f, t) <- ft; a <- comparable; b <- type_;
         return (f, M.Type (M.T_big_map a b) t)
  where
    ft = runPermutation $
      (,) <$> toPermutationWithDefault  def     fp
          <*> toPermutationWithDefault Nothing noteT

-- Comparable Types
comparable :: Parser M.Comparable
comparable = let c = do ct <- ct; M.Comparable ct <$> noteTDef in parens c <|> c

ct :: Parser M.CT
ct = (symbol "int" >> return M.T_int)
  <|> (symbol "nat" >> return M.T_nat)
  <|> (symbol "string" >> return M.T_string)
  <|> (symbol "bytes" >> return M.T_bytes)
  <|> (symbol "mutez" >> return M.T_mutez)
  <|> (symbol "bool" >> return M.T_bool)
  <|> (symbol "key_hash" >> return M.T_key_hash)
  <|> (symbol "timestamp" >> return M.T_timestamp)
  <|> (symbol "address" >> return M.T_address)

{- Operations Parsers -}
ops :: Parser [M.Op]
ops = braces $ sepEndBy (prim' <|> mac' <|> seq') semicolon
  where
    prim' = M.PRIM <$> try prim
    mac'  = M.MAC <$> try macro
    seq'  = M.SEQ <$> try ops

prim :: Parser M.I
prim = dropOp
  <|> dupOp
  <|> swapOp
  <|> pushOp
  <|> someOp
  <|> noneOp
  <|> unitOp
  <|> ifNoneOp
  <|> pairOp
  <|> carOp
  <|> cdrOp
  <|> leftOp
  <|> rightOp
  <|> ifLeftOp
  <|> ifRightOp
  <|> nilOp
  <|> consOp
  <|> ifConsOp
  <|> sizeOp
  <|> emptySetOp
  <|> emptyMapOp
  <|> mapOp
  <|> iterOp
  <|> memOp
  <|> getOp
  <|> updateOp
  <|> ifOp
  <|> loopLOp
  <|> loopOp
  <|> lambdaOp
  <|> execOp
  <|> dipOp
  <|> failWithOp
  <|> castOp
  <|> renameOp
  <|> concatOp
  <|> packOp
  <|> unpackOp
  <|> sliceOp
  <|> isNatOp
  <|> addressOp
  <|> addOp
  <|> subOp
  <|> mulOp
  <|> edivOp
  <|> absOp
  <|> negOp
  <|> modOp
  <|> lslOp
  <|> lsrOp
  <|> orOp
  <|> andOp
  <|> xorOp
  <|> notOp
  <|> compareOp
  <|> eqOp
  <|> neqOp
  <|> ltOp
  <|> leOp
  <|> gtOp
  <|> geOp
  <|> intOp
  <|> selfOp
  <|> contractOp
  <|> transferTokensOp
  <|> setDelegateOp
  <|> createAccountOp
  <|> createContract2Op
  <|> createContractOp
  <|> implicitAccountOp
  <|> nowOp
  <|> amountOp
  <|> balanceOp
  <|> checkSigOp
  <|> sha256Op
  <|> sha512Op
  <|> blake2BOp
  <|> hashKeyOp
  <|> stepsToQuotaOp
  <|> sourceOp
  <|> senderOp

{- Core instructions -}
-- Control Structures
failWithOp = do symbol "FAILWITH"; return M.FAILWITH
ifOp    = do symbol "IF"; a <- ops; M.IF a <$> ops
loopOp  = do symbol "LOOP"; M.LOOP <$> ops
loopLOp = do symbol "LOOP_LEFT"; M.LOOP_LEFT <$> ops
execOp  = do symbol "EXEC"; M.EXEC <$> noteVDef
dipOp   = do symbol "DIP"; M.DIP <$> ops

-- Stack Operations
dropOp   = do symbol "DROP"; return M.DROP;
dupOp    = do symbol "DUP"; M.DUP <$> noteVDef
swapOp   = do symbol "SWAP"; return M.SWAP;
pushOp   = do symbol "PUSH"; v <- noteVDef; a <- type_; M.PUSH v a <$> data_
unitOp   = do symbol "UNIT"; (t, v) <- notesTV; return $ M.UNIT t v
lambdaOp = do symbol "LAMBDA"; v <- noteVDef; a <- type_; b <- type_;
              M.LAMBDA v a b <$> ops

-- Generic comparison
eqOp  = do symbol "EQ"; M.EQ <$> noteVDef
neqOp = do symbol "NEQ"; M.NEQ <$> noteVDef
ltOp  = do symbol "LT"; M.LT <$> noteVDef
gtOp  = do symbol "GT"; M.GT <$> noteVDef
leOp  = do symbol "LE"; M.LE <$> noteVDef
geOp  = do symbol "GE"; M.GE <$> noteVDef

-- ad-hoc comparison
compareOp = do symbol "COMPARE"; M.COMPARE <$> noteVDef

{- Operations on Data -}
-- Operations on booleans
orOp  = do symbol "OR";  M.OR <$> noteVDef
andOp = do symbol "AND"; M.AND <$> noteVDef
xorOp = do symbol "XOR"; M.XOR <$> noteVDef
notOp = do symbol "NOT"; M.NOT <$> noteVDef

-- Operations on integers and natural numbers
addOp  = do symbol "ADD"; M.ADD <$> noteVDef
subOp  = do symbol "SUB"; M.SUB <$> noteVDef
mulOp  = do symbol "MUL"; M.MUL <$> noteVDef
edivOp = do symbol "EDIV";M.EDIV <$> noteVDef
absOp  = do symbol "ABS"; M.ABS <$> noteVDef
negOp  = do symbol "NEG"; return M.NEG;
modOp  = do symbol "MOD"; return M.MOD;

-- Bitwise logical operators
lslOp = do symbol "LSL"; M.LSL <$> noteVDef
lsrOp = do symbol "LSR"; M.LSR <$> noteVDef

-- Operations on strings
concatOp = do symbol "CONCAT"; M.CONCAT <$> noteVDef
sliceOp  = do symbol "SLICE"; M.SLICE <$> noteVDef

-- Operations on pairs
pairOp = do symbol "PAIR"; (t, v, (p, q)) <- notesTVF2; return $ M.PAIR t v p q
carOp  = do symbol "CAR"; (v, f) <- notesVF; return $ M.CAR v f
cdrOp  = do symbol "CDR"; (v, f) <- notesVF; return $ M.CDR v f

-- Operations on collections (sets, maps, lists)
emptySetOp = do symbol "EMPTY_SET"; (t, v) <- notesTV;
                M.EMPTY_SET t v <$> comparable
emptyMapOp = do symbol "EMPTY_MAP"; (t, v) <- notesTV; a <- comparable;
                M.EMPTY_MAP t v a <$> type_
memOp      = do symbol "MEM"; M.MEM <$> noteVDef
updateOp   = do symbol "UPDATE"; return M.UPDATE
iterOp     = do symbol "ITER"; v <- noteVDef; M.ITER v <$> ops
sizeOp     = do symbol "SIZE"; M.SIZE <$> noteVDef
mapOp      = do symbol "MAP"; v <- noteVDef; M.MAP v <$> ops
getOp      = do symbol "GET"; M.GET <$> noteVDef
nilOp      = do symbol "NIL"; (t, v) <- notesTV; M.NIL t v <$> type_
consOp     = do symbol "CONS"; M.CONS <$> noteVDef
ifConsOp   = do symbol "IF_CONS"; a <- ops; M.IF_CONS a <$> ops

-- Operations on options
someOp   = do symbol "SOME"; (t, v, f) <- notesTVF; return $ M.SOME t v f
noneOp   = do symbol "NONE"; (t, v, f) <- notesTVF; M.NONE t v f <$> type_
ifNoneOp = do symbol "IF_NONE"; a <- ops; M.IF_NONE a <$> ops

-- Operations on unions
leftOp    = do symbol "LEFT"; (t, v, (f, f')) <- notesTVF2;
               M.LEFT t v f f' <$> type_
rightOp   = do symbol "RIGHT"; (t, v, (f, f')) <- notesTVF2;
               M.RIGHT t v f f' <$> type_
ifLeftOp  = do symbol "IF_LEFT"; a <- ops; M.IF_LEFT a <$> ops
ifRightOp = do symbol "IF_RIGHT"; a <- ops; M.IF_RIGHT a <$> ops

-- Operations on contracts
createContractOp  = do symbol "CREATE_CONTRACT"; v <- noteVDef;
                       M.CREATE_CONTRACT v <$> noteVDef
createContract2Op = do symbol "CREATE_CONTRACT"; v <- noteVDef; v' <- noteVDef;
                       M.CREATE_CONTRACT2 v v' <$> braces contract
createAccountOp   = do symbol "CREATE_ACCOUNT"; v <- noteVDef; v' <- noteVDef;
                       return $ M.CREATE_ACCOUNT v v'
transferTokensOp  = do symbol "TRANSFER_TOKENS"; M.TRANSFER_TOKENS <$> noteVDef
setDelegateOp     = do symbol "SET_DELEGATE"; return M.SET_DELEGATE
balanceOp         = do symbol "BALANCE"; M.BALANCE <$> noteVDef
contractOp        = do symbol "CONTRACT"; M.CONTRACT <$> type_
sourceOp          = do symbol "SOURCE"; M.SOURCE <$> noteVDef
senderOp          = do symbol "SENDER"; M.SENDER <$> noteVDef
amountOp          = do symbol "AMOUNT"; M.AMOUNT <$> noteVDef
implicitAccountOp = do symbol "IMPLICIT_ACCOUNT"; M.IMPLICIT_ACCOUNT <$> noteVDef
selfOp            = do symbol "SELF"; M.SELF <$> noteVDef
addressOp         = do symbol "ADDRESS"; M.ADDRESS <$> noteVDef

-- Special Operations
nowOp          = do symbol "NOW"; M.NOW <$> noteVDef
stepsToQuotaOp = do symbol "STEPS_TO_QUOTA"; M.STEPS_TO_QUOTA <$> noteVDef

-- Operations on bytes
packOp   = do symbol "PACK"; M.PACK <$> noteVDef
unpackOp = do symbol "UNPACK"; v <- noteVDef; M.UNPACK v <$> type_

-- Cryptographic Primitives
checkSigOp = do symbol "CHECK_SIGNATURE"; M.CHECK_SIGNATURE <$> noteVDef
blake2BOp  = do symbol "BLAKE2B"; M.BLAKE2B <$> noteVDef
sha256Op   = do symbol "SHA256"; M.SHA256 <$> noteVDef
sha512Op   = do symbol "SHA512"; M.SHA512 <$> noteVDef
hashKeyOp  = do symbol "HASH_KEY"; M.HASH_KEY <$> noteVDef

{- Type operations -}
castOp = do symbol "CAST"; t <- noteTDef; M.CAST t <$> noteVDef
renameOp = do symbol "RENAME"; M.RENAME <$> noteVDef
isNatOp = do symbol "ISNAT"; return M.ISNAT
intOp = do symbol "INT"; M.INT <$> noteVDef

-- Macros
cmpOp = eqOp <|> neqOp <|> ltOp <|> gtOp <|> leOp <|> gtOp <|> geOp

macro :: Parser M.Macro
macro = do symbol "CMP"; a <- cmpOp; M.CMP a <$> noteVDef
  <|> do symbol "IFCMP"; a <- cmpOp; v <- noteVDef; b <- ops;
         M.IFCMP a v b <$> ops
  <|> do symbol "IF_SOME"; a <- ops; M.IF_SOME a <$> ops
  <|> do symbol "IF"; a <- cmpOp; bt <- ops; M.IFX a bt <$> ops
  <|> do symbol "FAIL"; return M.FAIL
  <|> do symbol "ASSERT_CMP"; M.ASSERT_CMP <$> cmpOp
  <|> do symbol "ASSERT_NONE"; return M.ASSERT_NONE
  <|> do symbol "ASSERT_SOME"; return M.ASSERT_SOME
  <|> do symbol "ASSERT_LEFT"; return M.ASSERT_LEFT
  <|> do symbol "ASSERT_RIGHT"; return M.ASSERT_RIGHT
  <|> do symbol "ASSERT_"; M.ASSERTX <$> cmpOp
  <|> do symbol "ASSERT"; return M.ASSERT
  <|> do string "DI"; n <- num "I"; symbol "P"; M.DIIP (n + 1) <$> ops
  <|> do string "DU"; n <- num "U"; symbol "P"; M.DUUP (n + 1) <$> noteVDef
  <|> pairMac
  <|> unpairMac
  <|> cadrMac
  <|> setCadrMac
  <|> mapCadrMac
  where
   num str = fromIntegral . length <$> some (string str)

pairMac :: Parser M.Macro
pairMac = do
  a <- pairMacInner
  symbol "R"
  (tn, vn, fns) <- permute3Def noteTDef noteV (some noteF)
  let ps = Macro.mapLeaves ((Nothing,) <$> fns) a
  return $ M.PAPAIR ps tn vn

pairMacInner :: Parser M.PairStruct
pairMacInner = do
  string "P"
  l <- (string "A" >> return (M.F (Nothing, Nothing))) <|> pairMacInner
  r <- (string "I" >> return (M.F (Nothing, Nothing))) <|> pairMacInner
  return $ M.P l r

unpairMac :: Parser M.Macro
unpairMac = do
  string "UN"
  a <- pairMacInner
  symbol "R"
  (vns, fns) <- permute2Def (some noteV) (some noteF)
  return $ M.UNPAIR (Macro.mapLeaves (zip vns fns) a)

cadrMac :: Parser M.Macro
cadrMac = lexeme $ do
  string "C"
  a <- some $ try $ cadrInner <* notFollowedBy (string "R")
  b <- cadrInner
  symbol "R"
  (vn, fn) <- notesVF
  return $ M.CADR (a ++ pure b) vn fn

cadrInner = (string "A" >> return M.A) <|> (string "D" >> return M.D)

setCadrMac :: Parser M.Macro
setCadrMac = do
  string "SET_C"
  a <- some cadrInner
  symbol "R"
  (v, f) <- notesVF
  return $ M.SET_CADR a v f

mapCadrMac :: Parser M.Macro
mapCadrMac = do
  string "MAP_C"
  a <- some cadrInner
  symbol "R"
  (v, f) <- notesVF
  M.MAP_CADR a v f <$> ops
