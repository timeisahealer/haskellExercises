toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | x < 10 && x > 0 = [x]
  | otherwise = toDigits (x`div`10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x < 10 = [x]
  | otherwise = [x `mod` 10] ++ toDigitsRev (x`div`10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = (doubleEveryOtherRev.reverse) x


doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:(y:zs)) = [x * 2, y] ++ doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : [])
  | x < 10 = x
  | otherwise = sumDigits(toDigits x)
sumDigits (x : xs) = (sumDigits.toDigits) x + (sumDigits) xs

validate :: Integer -> Bool
validate x
  | b `mod` 10 == 8 ||  b `mod` 10 == 0 = True
  | otherwise = False
  where
    b = (sumDigits.doubleEveryOtherRev.toDigitsRev) x
