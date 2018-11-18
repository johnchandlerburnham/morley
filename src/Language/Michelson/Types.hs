{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoImplicitPrelude         #-}

module Language.Michelson.Types where

import           Data.Bool
import qualified Data.ByteString as B
import           Data.Maybe
import           Data.Natural
import           Data.Sequence   as Seq
import qualified Data.Text       as T
import           Prelude         (Eq, Int, Integer, Ord, (++), (.))
import           Text.Show


-- smart contract
--data Contract = Contract Parameter Storage Code
--
---- parameter
--data Parameter = Parameter Type
--
--data Storage = Storage Type
--
--data Code = Code Ops

-- element of a map
data Element k v = Element (Data k) (Data v) deriving Show

-- data
data VarNote = NoVN | VN T.Text deriving Show

data Data t where
  Int        :: Integer -> VarNote -> Data (T_ct T_int)
  String     :: T.Text -> Data (T_ct T_string)
  Bytes      :: B.ByteString -> Data (T_ct T_bytes)
  Unit       :: VarNote -> Data T_unit
  True       :: VarNote -> Data (T_ct T_bool)
  False      :: VarNote -> Data (T_ct T_bool)
  Pair       :: VarNote -> Data a -> Data b -> Data (T_pair a b)
  Left       :: VarNote -> Data a -> Data (T_or a T_any)
  Right      :: VarNote -> Data a -> Data (T_or T_any a)
  Some       :: VarNote -> Data a -> Data (T_option ('Type a NoTN NoFN))
  None       :: VarNote -> Data (T_option ('Type T_any NoTN NoFN))
  --List       :: VarNote -> Seq (Data a) -> Data (T_list a)
  --Set        :: VarNote -> Seq (Data (T_ct a)) -> Data (T_set a)
  --Map        :: Seq (Element k v) -> Data (Element k v)
  -- DataOps    :: Ops -> Data Ops

data Any = forall a. Show a => Any a

instance Show Any where
  show (Any a) = "Any " ++ (show a)

instance Show (Data t) where
  show (Int n vn) = "Int " ++ (show n) ++ " " ++ (show vn)
  show _          = ""

data TypeNote  = NoTN | TN T.Text deriving Show
data FieldNote = NoFN | FN T.Text deriving Show

data Type = Type T TypeNote FieldNote deriving Show

--instance Show Type where
--  show (Type (T_comparable ct) tn fn) = (show ct) ++ (show tn) ++ (show fn)
--  show (Type t tn fn) = (show t) ++ (show tn) ++ (show fn)

data T where
  T_any        :: T
  T_ct         :: CT -> T
  T_key        :: T
  T_unit       :: T
  T_signature  :: T
  T_option     :: Type -> T
  T_list       :: T -> T
  T_set        :: CT -> T
  T_operation  :: T
  T_address    :: T
  T_contract   :: T -> T
  T_pair       :: T -> T -> T
  T_or         :: T -> T -> T
  T_lambda     :: T -> T -> T
  T_map        :: CT -> T -> T
  T_big_map    :: CT -> T -> T
  deriving Show

-- instance Show Comparable where
--   show (Comparable t tn) = (show t) ++ (show tn)

data CT where
  T_int       :: CT
  T_nat       :: CT
  T_string    :: CT
  T_bytes     :: CT
  T_mutez     :: CT
  T_bool      :: CT
  T_key_hash  :: CT
  T_timestamp :: CT
  deriving Show

{-
-- instruction sequence
data Ops = Ops { ops :: Seq Op }

opsConcat :: Ops -> Ops -> Ops
opsConcat x y = Ops ((ops x) Seq.>< (ops y))

(><) :: Ops -> Ops -> Ops
infixr 9 ><
(><) = opsConcat

opsLappend :: Op -> Ops -> Ops
opsLappend x y = Ops (x Seq.<| (ops y))

(<|) :: Op -> Ops -> Ops
infixr 9 <|
(<|) = opsLappend

opsRappend :: Ops -> Op -> Ops
opsRappend x y = Ops ((ops x) Seq.|> y)

(|>) :: Ops -> Op -> Ops
infixr 9 |>
(|>) = opsRappend

noOps :: Ops
noOps = Ops Seq.empty

opsFromList :: [Op] -> Ops
opsFromList = Ops . Seq.fromList

(|:) :: [Op] -> Ops
infixr 9 |:
(|:) = opsFromList

opsSingleton :: Op -> Ops
opsSingleton x = opsFromList [x]

-- instruction
data Op where
  OpsSeq            :: Ops -> Op
  DROP              :: Op
  DUP               :: VarNote -> Op
  SWAP              :: Op
  PUSH              :: VarNote -> Type -> Data a -> Op
  SOME              :: TypeNote -> VarNote -> FieldNote
                       -> Op
  NONE              :: TypeNote -> VarNote -> FieldNote
                       -> Type -> Op
  UNIT              :: TypeNote -> Op
  IF_NONE           :: Ops -> Ops -> Op
  PAIR              :: TypeNote -> VarNote -> FieldNote -> FieldNote
                       -> Op
  CAR               :: VarNote -> FieldNote -> Op
  CDR               :: VarNote -> FieldNote -> Op
  LEFT              :: TypeNote -> VarNote -> FieldNote -> FieldNote
                       -> Type -> Op
  RIGHT             :: TypeNote -> VarNote -> FieldNote -> FieldNote
                       -> Type -> Op
  IF_LEFT           :: Ops -> Ops -> Op
  IF_RIGHT          :: Ops -> Ops -> Op
  NIL               :: TypeNote -> VarNote -> Type -> Op
  CONS              :: VarNote -> Op
  IF_CONS           :: Ops -> Ops -> Op
  SIZE              :: VarNote -> Op
  EMPTY_SET         :: TypeNote -> VarNote -> Comparable -> Op
  EMPTY_MAP         :: TypeNote -> VarNote
                       -> Comparable -> Type -> Op
  MAP               :: VarNote -> Ops -> Op
  ITER              :: VarNote -> Ops -> Op
  MEM               :: VarNote -> Op
  GET               :: VarNote -> Op
  UPDATE            :: Op
  IF                :: Ops -> Ops -> Op
  LOOP              :: Ops -> Op
  LOOP_LEFT         :: Ops -> Op
  LAMBDA            :: VarNote -> Type -> Type -> Ops -> Op
  EXEC              :: VarNote -> Op
  DIP               :: Ops -> Op
  FAILWITH          :: Op
  CAST              :: TypeNote -> VarNote -> Op
  RENAME            :: VarNote -> Op
  CONCAT            :: VarNote -> Op
  SLICE             :: Op
  PACK              :: Op
  UNPACK            :: Op
  ADD               :: VarNote -> Op
  SUB               :: VarNote -> Op
  MUL               :: VarNote -> Op
  EDIV              :: VarNote -> Op
  ABS               :: VarNote -> Op
  NEG               :: Op
  MOD               :: Op
  LSL               :: VarNote -> Op
  LSR               :: VarNote -> Op
  OR                :: VarNote -> Op
  AND               :: VarNote -> Op
  NOT               :: VarNote -> Op
  COMPARE           :: VarNote -> Op
  EQ                :: VarNote -> Op
  NEQ               :: VarNote -> Op
  LT                :: VarNote -> Op
  GT                :: VarNote -> Op
  LE                :: VarNote -> Op
  GE                :: VarNote -> Op
  INT               :: VarNote -> Op
  SELF              :: VarNote -> Op
  TRANSFER_TOKENS   :: Op
  SET_DELEGATE      :: Op
  CREATE_ACCOUNT    :: VarNote -> VarNote -> Op
  CREATE_CONTRACT   :: VarNote -> VarNote -> Op
  CREATE_CONTRACT2  :: VarNote -> VarNote -> Ops -> Op
  IMPLICIT_ACCOUNT  :: VarNote -> Op
  NOW               :: VarNote -> Op
  AMOUNT            :: VarNote -> Op
  BALANCE           :: VarNote -> Op
  CHECK_SIGNATURE   :: VarNote -> Op
  BLAKE2B           :: VarNote -> Op
  HASH_KEY          :: VarNote -> Op
  STEPS_TO_QUOTA    :: VarNote -> Op
  SOURCE            :: VarNote -> Op
  SENDER            :: VarNote -> Op
  ADDRESS           :: VarNote -> Op
-}
-- type

