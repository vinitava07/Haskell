trocaP _ [] = []
trocaP c (x : xs)
  | x == c = '1' : (trocaP c xs)
  | otherwise = x : trocaP c xs

separaElem a [] = a
separaElem a (b : bs)
  | b `elem` a = separaElem a bs
  | otherwise = separaElem (a ++ [b]) bs

contar _ [] = 0
contar a (b : bs)
  | a == b = 1 + contar a bs
  | otherwise = 0 + contar a bs

aplicaContar [] _ = []
aplicaContar (a : as) (x) = (contar a x) : aplicaContar as x

funcaoBonita s = zip (aplicaContar (separaElem [] s) s) (separaElem [] s)
