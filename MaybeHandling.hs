module MaybeHandling where

-- x' <- Nothing is equivalent to x' = 0

fromMaybe :: a -> Maybe a -> a
fromMaybe z = maybe z id

maybeAdd :: Num a => Maybe a -> Maybe a -> Maybe a
maybeAdd x y = do 
    x' <- x
    y' <- y
    return (x' + y')

maybeSubtract ::  Num a => Maybe a -> Maybe a -> Maybe a
maybeSubtract x y = do 
    x' <- x
    y' <- y
    return (x' - y')

maybeDiv ::  Integral a => Maybe a -> Maybe a -> Maybe a
maybeDiv x y = do 
    x' <- x
    y' <- y
    return (x' `div` y')

maybeMultiply ::  Num a => Maybe a -> Maybe a -> Maybe a
maybeMultiply x y = do 
    x' <- x
    y' <- y
    return (x' * y')