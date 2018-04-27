toDigits :: Integer -> [Integer]
toDigits x
  | x < 10 = [x]
  | otherwise = toDigits (x`div`10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x < 10 = [x]
  | otherwise = [x `mod` 10] ++ toDigitsRev (x`div`10)
