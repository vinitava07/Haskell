replicate' 0 _ = []
replicate' x y = y : (replicate (x - 1) y)

take' 0 _ = []
take' n (x : xs)
  | n <= 0 = []
  | n > length (x : xs) = error "Erro numero maior que a lista"
  | otherwise = x : (take' (n - 1) xs)

reverse' [] = []
reverse' [a] = [a]
reverse' (x : xs) = reverse xs ++ [x]

elem' _ [] = False
elem' n (x : xs)
  | n == x = True
  | otherwise = elem' n xs

zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys