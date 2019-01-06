module MyUtils.Read where

{-|
  Given a String, maybe return a value of type a.  The type you wish to read
  must be specified somewhere in your program.
-}
maybeRead :: Read a => String -> Maybe a
maybeRead str =
  case reads str of
    [(a,_)] -> Just a
    _ -> Nothing
