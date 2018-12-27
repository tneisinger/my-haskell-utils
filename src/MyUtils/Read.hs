module MyUtils.Read where

maybeRead :: Read a => String -> Maybe a
maybeRead str =
  case reads str of
    [(a,_)] -> Just a
    _ -> Nothing
