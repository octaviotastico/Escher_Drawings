module Pred where
import Dibujo

type Pred a = a -> Bool

-- dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
limpia:: Pred a -> a -> Dibujo a -> Dibujo a
limpia p a d = sem (\x -> if p x then pureDibe a else pureDibe x) rotar rot45 espejar apilar juntar encimar d

-- alguna básica satisface el predicado
anyDib :: Pred a -> Dibujo a -> Bool
anyDib f (Juntar x y a b) = anyDib f a || anyDib f b
anyDib p d = sem p id id id predor predor (||) d
    where
        predor = \_ _ a b -> (||) a b

-- todas las básicas satisfacen el predicado
allDib :: Pred a -> Dibujo a -> Bool
allDib p d = sem p id id id predor predor (&&) d
    where
        predor = \_ _ a b -> (&&) a b

-- describe la figura. Ejemplos: 
--   desc (Basica b) (const "b") = "b"
--   desc (Rotar fa) db = "rot (" ++ desc fa db ++ ")"
-- la descripción de cada constructor son sus tres primeros
-- símbolos en minúscula.

drot :: String -> String
drot s = "rot (" ++ s ++ ")"

drot45 :: String -> String
drot45 s = "rot45 (" ++ s ++ ")"

desp :: String -> String
desp s = "esp (" ++ s ++ ")"

dapi :: Int -> Int -> String -> String -> String
dapi x y s1 s2 = "api " ++ show x ++ " " ++ show y ++ " (" ++ s1 ++ ", " ++ s2 ++ ")"

djun :: Int -> Int -> String -> String -> String
djun x y s1 s2 = "jun " ++ show x ++ " " ++ show y ++ " (" ++ s1 ++ ", " ++ s2 ++ ")"

denc :: String -> String -> String
denc s1 s2 = "enc (" ++ s1 ++ ", " ++ s2 ++ ")"

desc :: (a -> String) -> Dibujo a -> String
desc f d = sem f drot drot45 desp dapi djun denc d

-- junta todas las figuras básicas de un dibujo
every :: Dibujo a -> [a]
every d = sem (: []) id id id predcon predcon (++) d
    where
        predcon = \ _ _ xs ys -> xs ++ ys

-- auxiliar para contar
cnt :: Eq a => [a] -> [(a, Int)]
cnt [] = []
cnt (x:xs) = (x, 1 + length (filter (\a -> a == x) xs)) : (cnt (filter (\a -> a /= x) xs))

-- cuenta la cantidad de veces que aparecen las básicas en una figura.
contar :: Eq a => Dibujo a -> [(a, Int)]
contar d = cnt (every d)

cmp :: Integer -> Integer -> Integer
cmp x y = if y > x then y else 0

-- hay 4 rotaciones seguidas (empezando en el tope)
-- las rotaciones deben sumar 360º
esRot360 :: Pred (Dibujo a)
esRot360 d = (sem (const 0) (+ 1) (cmp x) (cmp x) prot1 prot1 prot2 d) > x
    where
        x = 3
        prot1 = \ _ _ d1 d2 -> (cmp x d1) + (cmp x d2)
        prot2 = \ d1 d2 -> (cmp x d1) + (cmp x d2)

-- hay 2 espejados seguidos (empezando en el tope)
esFlip2 :: Pred (Dibujo a)
esFlip2 d = (sem (const 0) (cmp x) (cmp x) (+ 1) pflip1 pflip1 pflip2 d) > x
    where
        x = 1
        pflip1 = \ _ _ d1 d2 -> cmp x d1 + cmp x d2
        pflip2 = \ d1 d2 -> cmp x d1 + cmp x d2

-- la cadena que se toma como parámetro es la descripción del error.
check :: Pred (Dibujo a) -> String -> Dibujo a -> Either String (Dibujo a)
check p s d = if (p d) then Left s else Right d

-- aplica todos los chequeos y acumula todos los errores,
-- sólo devuelve la figura si no hubo ningún error.
todoBien :: Dibujo a -> Either [String] (Dibujo a)
todoBien d = if err == [] then Right d else Left err
    where
        err = filter (\x -> x /= "") (map (\x -> case x of
                                        Left s -> s
                                        Right d -> "") checks)
            where checks = [check esRot360 "Hay 4 rotaciones seguidas" d,
                            check esFlip2 "Hay 2 flips seguidos" d]
    
borrarRot360:: Dibujo a -> Dibujo a
borrarRot360 (Rotar(Rotar(Rotar(Rotar(d))))) = d

noRot360 :: Dibujo a -> Dibujo a
noRot360 d = fst (sem f1 f2 f3 f4 f5 f6 f7 d)
            where
                f1 = \d1 -> (pureDibe d1, 0)
                f2 = \(d1, c) -> if(c + 1 == 4) then (borrarRot360 (rotar d1), 0) else (rotar d1, c + 1)
                f3 = \(d1, c) -> (rot45 d1, 0)
                f4 = \(d1, c) -> (espejar d1, 0)
                f5 = \x y (d1, c1) (d2, c2) -> (apilar x y d1 d2, 0)
                f6 = \x y (d1, c1) (d2, c2) -> (juntar x y d1 d2, 0)
                f7 = \(d1, c1) (d2, c2) -> (encimar d1 d2, 0)

borrarFlip2 :: Dibujo a -> Dibujo a
borrarFlip2 (Espejar(Espejar(d))) = d

noFlip2  :: Dibujo a -> Dibujo a
noFlip2 d = fst (sem f1 f2 f3 f4 f5 f6 f7 d)
            where
                f1 = \d1 -> (pureDibe d1, 0)
                f2 = \(d1, c) -> (rotar d1, 0)
                f3 = \(d1, c) -> (rot45 d1, 0)
                f4 = \(d1, c) -> if(c + 1 == 2) then (borrarFlip2 (espejar d1), 0) else (espejar d1, c + 1)
                f5 = \x y (d1, c1) (d2, c2) -> (apilar x y d1 d2, 0)
                f6 = \x y (d1, c1) (d2, c2) -> (juntar x y d1 d2, 0)
                f7 = \(d1, c1) (d2, c2) -> (encimar d1 d2, 0)