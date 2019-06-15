module Dibujo where

data Dibujo a = Bas a
              | Rotar (Dibujo a)
              | Rot45 (Dibujo a)
              | Espejar (Dibujo a)
              | Apilar Int Int (Dibujo a) (Dibujo a) 
              | Juntar Int Int (Dibujo a) (Dibujo a) 
              | Encimar (Dibujo a) (Dibujo a)
              deriving (Show, Eq)

-- Rotar 45 grados
rot45:: Dibujo a -> Dibujo a
rot45 d = Rot45 d

-- Rotar 90 grados
rotar:: Dibujo a -> Dibujo a
rotar d = Rotar d

-- Rotar 180 grados
r180 :: Dibujo a -> Dibujo a
r180 d = comp rotar 2 d

-- Rotar 270 grados
r270 :: Dibujo a -> Dibujo a
r270 d = comp rotar 3 d

-- Espejar
espejar:: Dibujo a -> Dibujo a
espejar d = Espejar d

-- Apilar
apilar:: Int -> Int -> Dibujo a -> Dibujo a -> Dibujo a
apilar a b d1 d2 = Apilar a b d1 d2

-- Juntar
juntar:: Int -> Int -> Dibujo a -> Dibujo a -> Dibujo a
juntar a b d1 d2 = Juntar a b d1 d2

-- Encimar
encimar:: Dibujo a -> Dibujo a -> Dibujo a
encimar d1 d2 = Encimar d1 d2

-- Compone n veces una función
comp :: (a -> a) -> Int -> a -> a
comp f 0 = id
comp f n = f . comp f (n - 1)

-- Pone una figura sobre la otra
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) d1 d2 = apilar 1 1 d1 d2

-- Pone una figura al lado de la otra
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) d1 d2 = juntar 1 1 d1 d2

-- Superpone una figura con otra
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) d1 d2 = encimar d1 d2

-- Repite una figura en los cuatro cuadrantes
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto d1 d2 d3 d4 = (.-.) ((///) d1 d2) ((///) d3 d4)

-- Repite una figura con cuatro rotaciones, superpuestas
encimar4 :: Dibujo a -> Dibujo a
encimar4 d = (^^^) ((^^^) d (rotar d)) ((^^^) (r180 d) (r270 d))

-- Repite una figura con 4 rotaciones en los 4 cuadrantes
ciclar :: Dibujo a -> Dibujo a
ciclar d = cuarteto d (rotar d) (r270 d) (r180 d)

-- Interpretar 'a' como una figura
pureDibe :: a -> Dibujo a
pureDibe x = Bas x

-- Mapea funciones a todas las figuras de un dibujo
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f d = case d of
    Bas a          -> pureDibe (f a)
    Rotar a        -> rotar (mapDib f a)
    Rot45 a        -> rot45 (mapDib f a)
    Espejar a      -> espejar (mapDib f a)
    Apilar x y a b -> apilar x y (mapDib f a) (mapDib f b)
    Juntar x y a b -> juntar x y (mapDib f a) (mapDib f b)
    Encimar a b    -> encimar (mapDib f a) (mapDib f b)

cambia :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
cambia f d = case d of
    Bas a          -> f a
    Rotar a        -> rotar (cambia f a)
    Rot45 a        -> rot45 (cambia f a)
    Espejar a      -> espejar (cambia f a)
    Apilar x y a b -> apilar x y (cambia f a) (cambia f b)
    Juntar x y a b -> juntar x y (cambia f a) (cambia f b)
    Encimar a b    -> encimar (cambia f a) (cambia f b)

-- estructura general para la semántica
sem :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Int -> Int -> b -> b -> b) -> 
       (Int -> Int -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b
sem bas rot r45 esp api jun enc d = case d of
    Bas a          -> bas a
    Rotar a        -> rot (sem bas rot r45 esp api jun enc a)
    Rot45 a        -> r45 (sem bas rot r45 esp api jun enc a)
    Espejar a      -> esp (sem bas rot r45 esp api jun enc a)
    Apilar x y a b -> api x y (sem bas rot r45 esp api jun enc a) (sem bas rot r45 esp api jun enc b)
    Juntar x y a b -> jun x y (sem bas rot r45 esp api jun enc a) (sem bas rot r45 esp api jun enc b)
    Encimar a b    -> enc (sem bas rot r45 esp api jun enc a) (sem bas rot r45 esp api jun enc b)
