-- Prática 04 de Haskell
-- Nome: Bianca Sabrina Bublitz

-- 01
faixaIdoso :: Int -> String
faixaIdoso idade
  | idade >= 60 && idade <= 64 = "IDO64"
  | idade >= 65 && idade <= 69 = "IDO69"
  | idade >= 70 && idade <= 74 = "IDO74"
  | idade >= 80 = "IDO80"
  | otherwise = "ND"

-- 02
classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos x = [ (a,b,faixaIdoso b) | (a,b) <- x ]

--03
classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' lista = map (\x -> (fst x, snd x, faixaIdoso(snd x))) lista

-- 04
--strColor :: (Int,Int,Int) -> String
--strColor cor = 