module MyUtils.Time where

import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
import Data.Time.Calendar (toGregorian)

import MyUtils.Show (showIntegralWZeros)

data DateFormat = YYYYMMDD | YYYYDDMM | YYMMDD | YYDDMM
  deriving (Eq, Show)

getDateFromUTCTime :: Integral a => UTCTime -> (a, a, a)
getDateFromUTCTime t =
  let (y, m ,d) = toGregorian . utctDay $ t
   in (fromIntegral y, fromIntegral m, fromIntegral d)

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

getTodayStr :: DateFormat -> String -> IO String
getTodayStr fmt sep =
  getCurrentTime >>= return . makeStrFromUTCTime fmt sep
