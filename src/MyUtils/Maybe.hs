module MyUtils.Maybe where

{-|
  This function abstracts out the common if-then-else pattern
  when working with Maybe values.  Given a Bool @b@ and a value
  of any type @result@, return @Just result@ if @b@ is True,
  @Nothing@ if @b@ is False.

  Example:

    > let f x = justIf (x > 3) x
    > f 5
    Just 5

    >f 2
    Nothing
-}
justIf :: Bool -> a -> Maybe a
justIf b result = if b then Just result else Nothing
