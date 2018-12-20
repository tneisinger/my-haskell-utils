module MyUtils.Maybe where

justIf :: Bool -> a -> Maybe a
justIf b result = if b then Just result else Nothing
