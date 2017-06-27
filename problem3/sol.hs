sieve :: Int -> [Int] -- returns prime factorization of number
isqrt = floor . sqrt . fromIntegral -- returns nearest integer of sqrt, rounded down
sieve 1 = []
sieve n
  | null factors = [n] -- prime
  | otherwise = factors ++ sieve (div n $ head factors)
  where factors = take 1 $ filter (\x -> mod n x == 0) [2 .. isqrt n]
main = print $ sieve 600851475143
