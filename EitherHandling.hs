module EitherHandling where

import Data.Either -- need to acknowledge this?

eitherGetIntegral :: (RealFrac a, Integral b) => Either String a -> Either String b
eitherGetIntegral (Left x) = Left x -- only called after confirming that it is a Right value, this line is just for completeness 
eitherGetIntegral y = do 
    y1 <- y
    let y2 = truncate y1 in 
            return y2  

eitherAdd :: Num r => Either String r -> Either String r -> Either String r
eitherAdd x y = case length (rights [x, y]) of
    2 -> do
        x' <- x
        y' <- y
        return (x' + y')
    _ -> Left (eitherGetErrors x y)

eitherSubtract ::  Num r => Either String r -> Either String r -> Either String r
eitherSubtract x y = case length (rights [x, y]) of
    2 -> do
        x' <- x
        y' <- y
        return (x' - y')
    _ -> Left (eitherGetErrors x y)

eitherDivide ::  Fractional r => Either String r -> Either String r -> Either String r
eitherDivide x y = case length (rights [x, y]) of
    2 -> do
        x' <- x
        y' <- y
        return (x' / y')
    _ -> Left (eitherGetErrors x y)

eitherMultiply ::  Num r => Either String r -> Either String r -> Either String r
eitherMultiply x y = case length (rights [x, y]) of
    2 -> do
        x' <- x
        y' <- y
        return (x' * y')
    _ -> Left (eitherGetErrors x y)

eitherDiv :: (RealFrac a, Num b) => Either String a -> Either String a -> Either String b
eitherDiv x y = case length (rights [x, y]) of
    2 -> do
        x1 <- eitherGetIntegral x 
        y1 <- eitherGetIntegral y  
        return (fromIntegral (x1 `div` y1))

eitherModulus :: (RealFrac a, Num b) => Either String a -> Either String a -> Either String b
eitherModulus x y = case length (rights [x, y]) of
    2 -> do
        x1 <- eitherGetIntegral x 
        y1 <- eitherGetIntegral y  
        return (fromIntegral (x1 `mod` y1))
    _ -> Left (eitherGetErrors x y)

eitherPower :: RealFrac a => Either String a -> Either String a -> Either String a
eitherPower x y = case length (rights [x, y]) of
    2 -> do
        x1 <- x 
        y1 <- eitherGetIntegral y  
        return (x1 ^ y1)
    _ -> Left (eitherGetErrors x y)

eitherAbsolute :: Num a => Either String a -> Either String a
eitherAbsolute x = case length (rights [x]) of
    1 -> do
        x1 <- x 
        return (abs x1)
    _ -> do 
        x1 <- x
        return x1

eitherGetErrors :: Either String a -> Either String a -> String
eitherGetErrors x y | isLeft x && isLeft y = 
                        "Errors detected: " ++
                        "1. " ++ fromLeft "?" x ++
                        "; 2." ++ fromLeft "?" y ++ 
                        ";"
                    | isLeft x = 
                        "Errors detected: " ++
                        "1. " ++ fromLeft "?" x ++ 
                        ";"
                    | isLeft y = 
                        "Errors detected: " ++
                        "1. " ++ fromLeft "?" y ++ 
                        ";"
    