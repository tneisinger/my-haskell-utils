module MyUtils.Maybe where

maybeIf :: Bool -> a -> Maybe a
maybeIf b result = if b then Just result else Nothing
