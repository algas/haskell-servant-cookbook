module Teenage
    ( Teenage
    , generateTeenage
    , teenage
    )where

newtype Teenage = Teenage { teenage :: Integer }
    deriving (Read, Show, Eq, Ord)

generateTeenage :: Integer -> Maybe Teenage
generateTeenage x
    | 12 <= x && x < 20 = Just $ Teenage x
    | otherwise = Nothing
