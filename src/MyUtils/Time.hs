module MyUtils.Time where

import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
import Data.Time.Calendar (toGregorian)

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
  getCurrentTime >>= return . makeStrFromUTCTime fmt sep
