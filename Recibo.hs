--Nome: Otávio Malta Borges
type Nome = String
type Preco = Int
type CodBar = Int
type BaseDeDados = [(CodBar,Nome,Preco)]
type ListaDeCodigos = [CodBar]
type Recibo = [(Nome,Preco)]

listaProdutos :: BaseDeDados
listaProdutos = [(1234,"Oleo DoBom, 1l",195),
 (4756,"Chocolate Cazzeiro, 250g",180),
 (3216,"Arroz DoBom, 5Kg",213),
 (5823,"Balas Pedregulho, 1Kg",379),
 (4719,"Queijo Mineirin, 1Kg",449),
 (6832,"Iogurte Maravilha, 1Kg",499),
 (1112,"Rapadura QuebraDente, 1Kg",80),
 (1111,"Sal Donorte, 1Kg",221),
 (1113,"Cafe DoBom, 1kg",285),
 (1115,"Biscoito Bibi, 1Kg",80),
 (3814,"Sorvete QGelo, 1l",695)]

tamLinha :: Int
tamLinha = 30

formataCentavos :: Preco -> String
formataCentavos p = real(p) ++ "." ++ centavos(p)
 where
  real :: Int -> String
  real num = show(num `div` 100)
  
centavos :: Int -> String
centavos num = show(num `rem` 100)

formataLinha :: (Nome,Preco) -> String
formataLinha(n,p) = n ++ getSeparator((tamLinha - (tamStr(n)+tamNum(p)))) ++ formataCentavos(p)

getSeparator :: Int -> String
getSeparator x
 | x == 0 = ""
 | x<=2 = " "
 | otherwise = '.' : getSeparator (x-1)

tamStr :: String -> Int
tamStr n = length n

tamNum :: Int -> Int
tamNum x = length(show(x))

formataLinhas :: [(Nome,Preco)] -> String
formataLinhas [] = ""
formataLinhas (x:xs) = formataLinha(x)++ "\n" ++ formataLinhas(xs)

geraTotal :: Recibo -> Preco
geraTotal [] = 0
geraTotal ((n,p):xs) = p + geraTotal(xs)

formataTotal :: Preco -> String                                                      
formataTotal t = "Total" ++ getSeparator(tamLinha - (length("Total") + tamNum(t) + 1) )++ "$" ++formataCentavos(t)++"\n"

formataRecibo :: Recibo -> String
formataRecibo ((n,p):xs) = "Supermercado QLegal \n" ++ formataLinhas(((n,p):xs)) ++ formataTotal(geraTotal(((n,p):xs)))
 where
acha :: BaseDeDados -> CodBar -> (Nome,Preco)
acha [] _ = ("Null",0)
acha ((c,n,p):xs) cBar
 | c == cBar = (n,p)
 | otherwise = acha xs cBar

achaItem :: CodBar -> (Nome, Preco)
achaItem i = acha listaProdutos i

fazRecibo :: ListaDeCodigos -> Recibo
fazRecibo [] = []
fazRecibo (x:xs) = achaItem(x) : fazRecibo xs

geraRecibo :: ListaDeCodigos -> String
geraRecibo lc = formataRecibo(fazRecibo lc)

main :: IO ()
main = do
 putStr(geraRecibo [1234,3216,4719,1112,1113,3814])
 y <- getLine
 putStrLn("End")