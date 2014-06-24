module NewBase60 where
import qualified Data.Map.Strict as Map

digits = "0123456789ABCDEFGHJKLMNPQRSTUVWXYZ_abcdefghijkmnopqrstuvwxyz"

itob60 :: Int -> String
itob60 0 = "0"
itob60 n = reverse $ newBase60' n
  where
    newBase60' 0 = ""
    newBase60' n = nextDigit n : newBase60' (n `div` 60)
    nextDigit n  = digits!!(n `mod` 60)

b60tot :: String -> Int
b60toi s =  sum $ zipWith (*) pValues $ reverse dValues
  where
    -- inxex values of the digit list (the base 10 value of the digit)
    indexes = Map.fromList $ zip digits [0..]
    -- TODO: accept confusible digits as representing the appropriate value.
    --       for instance intrepret "O" as the same value as "0"

    -- place values [60**0, 60**1, 60**2 ... ]
    pValues = map (60^) [0..]

    -- digit values of the given string s. Map lookup across the inxexes
    dValues = map (indexes Map.!) s

-- aliases to match Tantek's names
itosxg = itob60
sxgtoi = b60toi

-- TODO implement the 0 padded version
