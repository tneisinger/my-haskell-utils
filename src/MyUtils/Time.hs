{-|
Module      : MyUtils.Time
Description : Useful functions involving times or dates
Copyright   : (c) Tyler Neisinger, 2018
License     : GPL-3
Maintainer  : tjneisi@gmail.com
Stability   : experimental
Portability : POSIX

This module contains useful functions for working with times or dates.
-}
module MyUtils.Time where

import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
import Data.Time.Calendar (toGregorian, Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale, ParseTime)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), localTimeToUTC,
                            getCurrentTimeZone, midnight)
import Control.Monad.Fail

import MyUtils.Show (showIntegralWZeros)

{-|
  This type is used to specify the format of a date string.
-}
data DateFormat = YYYYMMDD | YYYYDDMM | YYMMDD | YYDDMM
  deriving (Eq, Show)

{-|
  Given a UTCTime, return the (year, month, day)
-}
getDateFromUTCTime :: Integral a => UTCTime -> (a, a, a)
getDateFromUTCTime t =
  let (y, m ,d) = toGregorian . utctDay $ t
   in (fromIntegral y, fromIntegral m, fromIntegral d)

{-|
  Given a DateFormat, a separator string, and a UTCTime, return
  a string that represents the date of the given UTCTime value.
-}
makeStrFromUTCTime :: DateFormat -> String -> UTCTime -> String
makeStrFromUTCTime fmt sep t =
  let dateInts :: (Int, Int, Int)
      dateInts = getDateFromUTCTime t
      (y, m, d) = dateInts
      show2 = showIntegralWZeros 2
      (yStr, mStr, dStr) = (show y, show2 m, show2 d)
   in case fmt of
        YYYYMMDD -> intercalate sep [yStr, mStr, dStr]
        YYYYDDMM -> intercalate sep [yStr, dStr, mStr]
        YYMMDD   -> intercalate sep [drop 2 yStr, mStr, dStr]
        YYDDMM   -> intercalate sep [drop 2 yStr, dStr, mStr]

{-|
  Given a DateFormat and a separator string, return today's date
  in the requested format.
-}
getTodayStr :: DateFormat -> String -> IO String
getTodayStr fmt sep =
  makeStrFromUTCTime fmt sep <$> getCurrentTime

{-|
  Use this function to maybe read a date string or time string into a Day,
  LocalTime, or other time type.  The first argument specifies the expected
  format of the string that is to be read. The second argument is the string
  you wish to read into a time type.

  This function will strip leading and trailing whitespace in an effort to
  read the input.

  For full instructions on how to structure the format string, see the
  documentation for the haskell Data.Time.Format module.

  Examples:

    -- Parse a date if it is in "YYYY-MM-DD" format
    > mightParseTime "%F" "2019-01-11" :: Maybe Day
    Just 2019-01-11

    -- Parse a date where the month or day might be one digit
    > mightParseTime "%Y-%-m-%-d" "2019-1-11" :: Maybe Day
    Just 2019-01-11

    -- Parse a date if it is in "MM/DD/YY" format
    > mightParseTime "%D" "01/11/19" :: Maybe Day
    Just 2019-01-11

    -- Same as above but the month and day can be single digit
    > mightParseTime "%-m/%-d/%y" "1/5/19" :: Maybe Day
    Just 2019-01-05

    --  Parse "HH:MM:SS" into a TimeOfDay
    > mightParseTime "%T" "12:13:14" :: Maybe TimeOfDay
    Just 12:13:14

    -- Parse "HH:MM" into a TimeOfDay
    > mightParseTime "%R" "12:13" :: Maybe TimeOfDay
    Just 12:13:00
-}
mightParseTime :: (MonadFail m, ParseTime t) => String -> String -> m t
mightParseTime = parseTimeM True defaultTimeLocale

{-|
  Given maybe a TimeOfDay and a date string, maybe return a LocalTime.
  If Nothing is given for the TimeOfDay, midnight will be used as the time.
  The given date string should be of the form "YYYY-MM-DD"
-}
dateStrToLocTime :: Maybe TimeOfDay -> String -> Maybe LocalTime
dateStrToLocTime maybeToD dateStr =
  let timeOfDay = fromMaybe midnight maybeToD
      makeLocalTime day = Just $ LocalTime day timeOfDay
   in mightParseTime "%Y-%-m-%-d" dateStr >>= makeLocalTime

{-|
  Given maybe a TimeOfDay and a date string, maybe return a UTCTime.  This
  function must perform IO to get the current timezone, which is needed to
  create the UTCTime value.  If no TimeOfDay is provided, midnight will be used
  for the time of day. The given date string should be of the form "YYYY-MM-DD"
-}
dateStrToUTC :: Maybe TimeOfDay -> String -> IO (Maybe UTCTime)
dateStrToUTC maybeToD dateStr = do
  tz <- getCurrentTimeZone
  let maybeLocalTime = dateStrToLocTime maybeToD dateStr
  traverse (return . localTimeToUTC tz) maybeLocalTime

{-|
  Given a Day and a TimeOfDay, perform IO to return a correct UTCTime. This
  function must perform IO to get the current timezone, which is need to create
  the UTCTime value.
-}
dayAndTimeToUTC :: Day -> TimeOfDay -> IO UTCTime
dayAndTimeToUTC day timeOfDay = do
  tz <- getCurrentTimeZone
  return . localTimeToUTC tz $ LocalTime day timeOfDay

{-|
  Given a TimeOfDay, return a string that expresses the hour, minute, and am
  or pm.

  Example, if given a TimeOfDay for 16:05:25 (military time), this function
  would return: "4:05pm"
-}
timeOfDayToAmPmStr :: TimeOfDay -> String
timeOfDayToAmPmStr (TimeOfDay h m _) =
  case compare h 12 of
    LT -> show h ++ ":" ++ showIntegralWZeros 2 m ++ "am"
    EQ -> show h ++ ":" ++ showIntegralWZeros 2 m ++ "pm"
    GT -> show (h - 12) ++ ":" ++ showIntegralWZeros 2 m ++ "pm"
