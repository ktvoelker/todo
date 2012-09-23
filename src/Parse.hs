
module Parse where

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Text.Parsec
import Text.Parsec.Pos

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

parDateToTime :: ParDate -> Maybe UTCTime
parDateToTime ParDate{..} =
  case (dayOfWeek, pdDayOfWeek) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> u
    (Just act, Just exp) | act == exp -> u
    _ -> Nothing
  where
    day = liftM3 fromGregorian (fmap fromIntegral pdYear) pdMonth pdDayOfMonth
    dayOfWeek = fmap ((\(_, _, x) -> x) . toWeekDate) day
    u = fmap (flip UTCTime 0) day

parseTimeNow :: String -> IO (Maybe UTCTime)
parseTimeNow xs = liftM (parseTime xs) getCurrentTime

parseTime :: String -> UTCTime -> Maybe UTCTime
parseTime xs now =
    (>>= parDateToTime)
  . join
  . either (const Nothing) Just
  . runParser (time now) () ""
  . tokenize
  $ xs

p m = runParser m () "" . tokenize

pd m = fmap utctDay . p m

pdm m = fmap (fmap utctDay) . p m

now = getCurrentTime

data ParDate =
  ParDate
  { pdYear :: Maybe Int
  , pdMonth :: Maybe Int
  , pdDayOfMonth :: Maybe Int
  , pdDayOfWeek :: Maybe Int
  } deriving (Eq, Ord, Show)

timeToParDate :: UTCTime -> ParDate
timeToParDate u =
  ParDate
  { pdYear = Just . fromInteger $ year
  , pdMonth = Just month
  , pdDayOfMonth = Just dayOfMonth
  , pdDayOfWeek = Just dayOfWeek
  }
  where
    day = utctDay u
    (year, month, dayOfMonth) = toGregorian day
    (_, _, dayOfWeek) = toWeekDate day

mergeParDates :: ParDate -> ParDate -> ParDate
mergeParDates a b =
  ParDate
  { pdYear = pdYear a `mplus` pdYear b
  , pdMonth = pdMonth a `mplus` pdMonth b
  , pdDayOfMonth = pdDayOfMonth a `mplus` pdDayOfMonth b
  , pdDayOfWeek = pdDayOfWeek a `mplus` pdDayOfWeek b
  }

ite :: Bool -> a -> a -> a
ite b t f = if b then t else f

tokEq :: Token -> P u ()
tokEq a = tok (\b -> if a == b then Just () else Nothing)

addDuration :: Int -> Unit -> UTCTime -> UTCTime
addDuration n u t = UTCTime day' $ utctDayTime t
  where
    n' = fromIntegral n
    day = utctDay t
    day' = case u of
      Year -> addGregorianYearsRollOver n' day
      Month -> addGregorianMonthsRollOver n' day
      Week -> addDays (7 * n') day
      Day -> addDays n' day

data Unit = Year | Month | Week | Day deriving (Eq, Ord, Enum, Bounded, Show)

data Token =
    TNat Int
  | THoliday ParDate
  | TDayOfWeek Int
  | TMonthName Int
  | TDigitWord Int
  | TUnit Unit
  | TRel Ordering
  | TNext
  | TIn
  deriving (Eq, Ord, Show)

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

isHoliday :: Token -> Maybe ParDate
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

splitTokens :: String -> [(String, SourcePos)]
splitTokens =
  map (\xs -> (map snd xs, newPos "" 1 . fst . head $ xs))
  . split (dropBlanks . condense . whenElt $ not . isAlphaNum . snd)
  . zip [1 ..]

tokenize :: String -> [((String, SourcePos), Token)]
tokenize =
  catMaybes
  . map (\t -> fmap (t, ) . parseToken . map toLower . fst $ t)
  . splitTokens

holidays :: [(String, ParDate)]
holidays =
  [ ( "christmas", ParDate Nothing (Just 12) (Just 25) Nothing )
  , ( "xmas", ParDate Nothing (Just 12) (Just 25) Nothing )
  ]

weekDays :: [(String, Int)]
weekDays =
  [ ( "su", 7 )
  , ( "mon", 1 )
  , ( "tu", 2 )
  , ( "wed", 3 )
  , ( "th", 4 )
  , ( "fr", 5 )
  , ( "sa", 6 )
  ]

unitNames :: [(String, Unit)]
unitNames =
  [ ( "y", Year )
  , ( "m", Month )
  , ( "month", Month )
  , ( "w", Week)
  , ( "wk", Week)
  , ( "week", Week )
  , ( "d", Day )
  ]

monthNames :: [(String, Int)]
monthNames =
  [ ( "ja", 1 )
  , ( "fe", 2 )
  , ( "mar", 3 )
  , ( "ap", 4 )
  , ( "may", 5 )
  , ( "jun", 6 )
  , ( "jul", 7 )
  , ( "au", 8 )
  , ( "se", 9 )
  , ( "o", 10 )
  , ( "n", 11 )
  , ( "de", 12 )
  ]

digitNames :: [(String, Int)]
digitNames =
  [ ( "one", 1 )
  , ( "two", 2 )
  , ( "three", 3 )
  , ( "four", 4 )
  , ( "five", 5 )
  , ( "six", 6 )
  , ( "seven", 7 )
  , ( "eight", 8 )
  , ( "nine", 9 )
  , ( "ten", 10 )
  , ( "eleven", 11 )
  , ( "twelve", 12 )
  ]

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

fixedWordTokens :: [(String, Token)]
fixedWordTokens =
  [ ( "bef", TRel LT )
  , ( "aft", TRel GT )
  , ( "next", TNext )
  , ( "in", TIn )
  ]

wordTokens :: [(String, Token)]
wordTokens =
     fixedWordTokens
  ++ map (mapSnd THoliday) holidays
  ++ map (mapSnd TDayOfWeek) weekDays
  ++ map (mapSnd TUnit) unitNames
  ++ map (mapSnd TMonthName) monthNames
  ++ map (mapSnd TDigitWord) digitNames

parseToken :: String -> Maybe Token
parseToken xs = msum [numMatch, wordMatch]
  where
    numMatch =
      fmap (TNat . fst)
      . listToMaybe
      . reads
      $ xs
    wordMatch =
      listToMaybe
      . map snd
      . sortBy (\a b -> compare (l b) (l a))
      . filter ((`isPrefixOf` xs) . fst)
      $ wordTokens
    l = length . fst

type P u a = Parsec [((String, SourcePos), Token)] u a

tok :: (Token -> Maybe a) -> P u a
tok = token (fst . fst) (snd . fst) . (. snd)

applyLimit :: ParDate -> (Ordering, ParDate) -> Maybe ParDate
-- TODO
applyLimit = const . Just

-- time ::= time-with-limit | limit
time :: UTCTime -> P u (Maybe ParDate)
time now =
  (liftM (applyLimit $ timeToParDate now) (limit now))
  <|>
  timeWithLimit now

-- time-with-limit ::= (absolute-time | fuzzy-time | relative-time) limit?
timeWithLimit :: UTCTime -> P u (Maybe ParDate)
timeWithLimit now = do
  pd <- timeNoLimit now
  liftM (maybe (Just pd) (applyLimit pd)) (optionMaybe $ limit now)

timeNoLimit :: UTCTime -> P u ParDate
timeNoLimit now =
  fmap timeToParDate (fuzzyTime now <|> relativeTime now)
  <|>
  absoluteTime now

-- absolute-time ::= time-token*
absoluteTime :: UTCTime -> P u ParDate
absoluteTime now = tok isHoliday <|> basic now

basic :: UTCTime -> P u ParDate
basic now = do
  pd <- liftM processBasic . many1 . tok $ basicToken
  case pd of
    Nothing -> mzero
    Just (ParDate Nothing Nothing Nothing (Just dow)) ->
      return . timeToParDate . nextDayOfWeek now $ dow
    Just pd -> return pd

processBasic :: [Basic] -> Maybe ParDate
processBasic = foldr f . Just $ ParDate Nothing Nothing Nothing Nothing
  where
    chk field rec = maybe (Just rec) (const Nothing) field
    f _ Nothing = Nothing
    f tok (Just pd@ParDate{..}) = case tok of
      BMonthName n -> chk pdMonth $ pd { pdMonth = Just n }
      BDayOfWeek n -> chk pdDayOfWeek $ pd { pdDayOfWeek = Just n }
      BNat n -> case (pdYear, pdMonth, pdDayOfMonth) of
        _ | n > 31 -> chk pdYear $ pd { pdYear = Just n }
        (Nothing, _, Nothing) -> chk pdDayOfMonth $ pd { pdDayOfMonth = Just n }
        (Nothing, Nothing, Just _) -> chk pdMonth $ pd { pdMonth = Just n }
        (Just _, Nothing, _) -> chk pdMonth $ pd { pdMonth = Just n }
        (Just _, Just _, Nothing) -> chk pdDayOfMonth $ pd { pdDayOfMonth = Just n }
        _ -> chk pdYear $ pd { pdYear = Just n }

-- fuzzy-time ::= 'next' (time-unit | day-of-week)
fuzzyTime :: UTCTime -> P u UTCTime
fuzzyTime now = do
  tokEq TNext
  (liftM (nextUnit now) (tok isUnit)) <|> (liftM (nextDayOfWeek now) (tok isDayOfWeek))

nextUnit :: UTCTime -> Unit -> UTCTime
nextUnit u Week = nextDayOfWeek u 1
nextUnit u Year = UTCTime (fromGregorian (year + 1) 1 1) 0
  where
    (year, _, _) = toGregorian . utctDay $ u
nextUnit u Month =
  UTCTime (addGregorianMonthsRollOver 1 $ fromGregorian year month 1) 0
  where
    (year, month, _) = toGregorian . utctDay $ u
nextUnit u Day = UTCTime (addDays 1 $ utctDay u) 0

nextDayOfWeek :: UTCTime -> Int -> UTCTime
nextDayOfWeek fromTime toDOW =
  UTCTime (addDays (fromIntegral diffDays) fromDay) (utctDayTime fromTime)
  where
    fromDay = utctDay fromTime
    (_, _, fromDOW) = toWeekDate fromDay
    diffDays = if toDOW <= fromDOW then 7 - fromDOW + toDOW else toDOW - fromDOW

-- relative-time ::= 'in'? duration+
relativeTime :: UTCTime -> P u UTCTime
relativeTime now = do
  tokEq TIn
  liftM (foldr (uncurry addDuration) now) (many1 duration)

-- duration ::= coefficient time-unit
duration :: P u (Int, Unit)
duration = liftM2 (,) coefficient (tok isUnit)

-- coefficient ::= digit-word | NAT
coefficient :: P u Int
coefficient = tok isDigitWord <|> try (tok isNat)

-- limit ::= ('before' | 'after') time
limit :: UTCTime -> P u (Ordering, ParDate)
limit now = liftM2 (,) (tok isRel) (timeNoLimit now)

