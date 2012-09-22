
module Parse where

import Control.Monad
import Data.Time.Clock
import Text.Parsec

{--
 - Productions based on big word lists: holiday, time-unit, day-of-week, month-name
 -
 - Note that when interpreting an absolute-time under a limit, non-specified fields
 - must be given either the max or the min, depending on whether the limit word is
 - 'after' or 'before'.
 -
 - To handle a time-token-list, sort the tokens by ambiguity (the number of fields
 - they could fill), then use a backtracking solver to assign them into fields.
 - As the last step, the solver should check whether the date is valid. Also, for each
 - token, it should have a particular order that it wants to try the fields in,
 - based on the position it appeared at in the list.
 -
 - First word: prefer day of week
 - Other words: prefer name of month
 -
 - Number nearest to any other number greater than 31: prefer month
 - Other numbers: prefer day of month
 -}

parseTime :: String -> UTCTime -> Maybe UTCTime
parseTime _ now = Just now

data ParDate =
  ParDate
  { pdYear :: Maybe Int
  , pdMonth :: Maybe Int
  , pdDayOfMonth :: Maybe Int
  , pdDayOfWeek :: Maybe Int
  } deriving (Eq, Ord, Show)

timeToParDate :: UTCTime -> ParDate
timeToParDate = undefined

parDateToTime :: UTCTime -> ParDate -> ParDate
parDateToTime _ = undefined

ite b t f = if b then t else f

tokEq a = tok (\b -> if a == b then Just () else Nothing)

addDuration :: Int -> Unit -> ParDate -> ParDate
addDuration = undefined

data Unit = Year | Month | Week | Day deriving (Eq, Ord, Enum, Bounded, Show)

data Token =
    TNat Int
  | THoliday UTCTime
  | TDayOfWeek Int
  | TMonthName Int
  | TDigitWord Int
  | TUnit Unit
  | TRel Ordering
  | TNext
  | TIn
  deriving (Eq, Ord)

data Basic =
    BNat Int
  | BDayOfWeek Int
  | BMonthName Int
  deriving (Eq, Ord, Show)

isNat :: Token -> Maybe Int
isNat (TNat n) = Just n
isNat _ = Nothing

basicToken :: Token -> Maybe Basic
basicToken (TNat n) = Just $ BNat n
basicToken (TDayOfWeek n) = Just $ BDayOfWeek n
basicToken (TMonthName n) = Just $ BMonthName n
basicToken _ = Nothing

isHoliday :: Token -> Maybe UTCTime
isHoliday (THoliday t) = Just t
isHoliday _ = Nothing

isDayOfWeek :: Token -> Maybe Int
isDayOfWeek (TDayOfWeek n) = Just n
isDayOfWeek _ = Nothing

isMonthName :: Token -> Maybe Int
isMonthName (TMonthName n) = Just n
isMonthName _ = Nothing

isDigitWord :: Token -> Maybe Int
isDigitWord (TDigitWord n) = Just n
isDigitWord _ = Nothing

isUnit :: Token -> Maybe Unit
isUnit (TUnit n) = Just n
isUnit _ = Nothing

isRel :: Token -> Maybe Ordering
isRel (TRel o) = Just o
isRel _ = Nothing

tokenize :: String -> [((String, SourcePos), Token)]
tokenize = undefined

type P u a = Parsec [((String, SourcePos), Token)] u a

tok :: (Token -> Maybe a) -> P u a
tok = token (fst . fst) (snd . fst) . (. snd)

applyLimit :: ParDate -> (Ordering, ParDate) -> ParDate
applyLimit _ = undefined

-- time ::= time-with-limit | limit
time now =
  (liftM (applyLimit now) (limit now))
  <|>
  timeWithLimit now

-- time-with-limit ::= (absolute-time | fuzzy-time | relative-time) limit?
timeWithLimit now =
  liftM2 applyLimit (absoluteTime <|> fuzzyTime now <|> relativeTime now) (limit now)

-- absolute-time ::= holiday | time-token*
absoluteTime =
  (liftM timeToParDate (tok isHoliday))
  <|>
  (liftM processBasic (many1 $ tok basicToken))

processBasic :: [Basic] -> ParDate
processBasic = undefined

-- fuzzy-time ::= 'next' (time-unit | day-of-week)
fuzzyTime now = do
  tokEq TNext
  (liftM (nextUnit now) (tok isUnit)) <|> (liftM (nextDayOfWeek now) (tok isDayOfWeek))

nextUnit :: ParDate -> Unit -> ParDate
nextUnit = undefined

nextDayOfWeek :: ParDate -> Int -> ParDate
nextDayOfWeek = undefined

-- relative-time ::= 'in'? duration+
relativeTime now = do
  tokEq TIn
  liftM (foldr (uncurry addDuration) now) (many1 duration)

-- duration ::= coefficient time-unit
duration = liftM2 (,) coefficient (tok isUnit)

-- coefficient ::= digit-word | NAT
coefficient = tok isDigitWord <|> tok isNat

-- limit ::= ('before' | 'after') time
limit now = liftM2 (,) (tok isRel) (time now)

