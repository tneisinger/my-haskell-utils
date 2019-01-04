module MyUtils.Time where

import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import MyUtils.Show (showIntegralWZeros)

getDate :: Num a => IO (a, a, a)
getDate = do
  (year, month, day) <- getCurrentTime >>= return . toGregorian . utctDay
  return (fromIntegral year, fromIntegral month, fromIntegral day)

getDateStringYYYYMMDD :: String -> IO String
getDateStringYYYYMMDD separator = do
  (year, month, day) <- getDate
  return $ intercalate  separator [ show year
                                  , showIntegralWZeros 2 month
                                  , showIntegralWZeros 2 day
                                  ]
