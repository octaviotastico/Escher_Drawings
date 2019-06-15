module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo
type FloatingPic = Vector -> Vector -> Vector -> Picture

type Output a = a -> FloatingPic

-- el vector nulo
zero :: Vector
zero = (0,0)

half :: Vector -> Vector
half = (0.5 V.*)

union :: Picture -> Picture -> Picture
union p q = pictures [p, q]

-- comprender esta función es un buen ejericio.
hlines :: Vector -> Float -> Float -> [Picture]
hlines v@(x,y) mag sep = map (hline . (*sep)) [0..]
    where hline h = line [(x,y+h),(x+mag,y+h)]

-- Una grilla de n líneas, comenzando en v con una separación de sep y
-- una longitud de l (usamos composición para no aplicar este
-- argumento)
grid :: Int -> Vector -> Float -> Float -> Picture
grid n v sep l = pictures [ls,translate 0 (l*toEnum n) (rotate 90 ls)]
    where ls = pictures $ take (n+1) $ hlines v sep l

-- figuras adaptables comunes
trian1 :: FloatingPic
trian1 a b c = line $ map (a V.+) [zero, half b V.+ c , b , zero]

trian2 :: FloatingPic
trian2 a b c = line $ map (a V.+) [zero, c, b,zero]

trianD :: FloatingPic
trianD a b c = line $ map (a V.+) [c, half b , b V.+ c , c]

rectan :: FloatingPic
rectan a b c = line [a, a V.+ b, a V.+ b V.+ c, a V.+ c,a]

simple :: Picture -> FloatingPic
simple p _ _ _ = p

blank :: FloatingPic
blank a b c = Blank

(x,y) .+ (x',y') = (x+x',y+y')
s .* (x,y) = (s*x,s*y)
(x,y) .- (x',y') = (x-x',y-y')
negar (x,y) = (-x,-y)

curvita :: FloatingPic
curvita a b c = line $ bezier a (a .+ b .+((1/3) .* c)) (a .+ b .+ c) 10

bezier :: Vector -> Vector -> Vector -> Int -> [Vector]
bezier p0 p1 p2 n = [ p1 .+ (((1-t)^2) .* (p0 .+ (negar p1))) .+ ((t^2) .* (p2 .+ (negar p1))) | t <- ts]
  where ts = 0:map (divF n) [1..n]
        divF :: Int -> Int -> Float
        divF j i = toEnum i / toEnum j

fShape :: FloatingPic
fShape a b c = line . map (a V.+) $ [ zero,uX, p13, p33, p33 V.+ uY , p13 V.+ uY 
                 , uX V.+ 4 V.* uY ,uX V.+ 5 V.* uY, x4 V.+ y5
                 , x4 V.+ 6 V.* uY, 6 V.* uY, zero]    
    where
        p33 = 3 V.* (uX V.+ uY)
        p13 = uX V.+ 3 V.* uY
        x4 = 4 V.* uX
        y5 = 5 V.* uY
        uX = (1/6) V.* b
        uY = (1/6) V.* c

-- Dada una función que produce una figura a partir de un a y un vector
-- producimos una figura flotante aplicando las transformaciones
-- necesarias. Útil si queremos usar figuras que vienen de archivos bmp.
transf :: (a -> Vector -> Picture) -> a -> Vector -> FloatingPic
transf f d (xs, ys) a b c  = translate (fst a') (snd a') .
                             scale (flip1 * (magV b/xs)) (flip2 * (magV c/ys)) .
                             rotate ang $ f d (xs, ys)

    where
        a' = a V.+ half (b V.+ c)
        thetab = radToDeg $ argV b
        thetac = radToDeg $ argV c
        caso = 
            if snd b >= 0 && snd c >= 0
                then
                    if thetab < thetac
                        then 0
                        else 1
            else if snd b < 0 && snd c < 0
                then
                    if thetac < thetab
                        then 2
                        else 0
            else if snd b >= 0 && snd c < 0
                then
                    if fst b < 0
                        then 0
                        else 3
            else if fst b >= 0
                then 0
                else 4
        getAng = \x t -> if snd x < 0 then t else -t
        flip1 = case caso of
            0 -> 1
            1 -> -1
            2 -> 1
            3 -> 1
            4 -> 1
        flip2 = case caso of
            0 -> 1
            1 -> 1
            2 -> -1
            3 -> -1
            4 -> -1
        ang = case caso of
            0 -> getAng b thetab
            1 -> getAng c thetac
            2 -> (getAng b thetab)
            3 -> -(getAng c thetac)
            4 -> getAng b thetab

                

buildescher :: [(Vector, Vector)] -> Color -> FloatingPic
buildescher coords col a b c = Color col (pictures . map (\(x, y) -> line[x, y]) $ newcoords)
    where
        newcoords = map (\((x1, y1), (x2, y2)) -> (((x1/16) V.* b) V.+ ((y1/16) V.* c) V.+ a, ((x2/16) V.* b) V.+ ((y2/16) V.* c) V.+ a)) $ coords
  
rotar:: FloatingPic -> FloatingPic
rotar fp a b c = fp (a V.+ b) c (zero V.- b)

rotar45:: FloatingPic -> FloatingPic
rotar45 fp a b c = fp (a V.+ (half (b V.+ c))) (half (b V.+ c)) (half (c V.- b))

espejar:: FloatingPic -> FloatingPic
espejar fp a b c = fp (a V.+ b) (V.negate b) c

apilar:: Int -> Int -> FloatingPic -> FloatingPic -> FloatingPic
apilar n m p q a b c = union (q (a V.+ c') b (r V.* c)) (p a b c')
    where
        t = fromIntegral (n + m)
        r' = (fromIntegral n) / t
        r = (fromIntegral m) / t
        c' = r' V.* c

juntar:: Int -> Int -> FloatingPic -> FloatingPic -> FloatingPic
juntar n m p q a b c = union (q a b' c) (p (a V.+ b') (r' V.* b) c)
    where
        t = fromIntegral (n + m)
        r' = (fromIntegral n) / t
        r = (fromIntegral m) / t
        b' = r V.* b

encimar:: FloatingPic -> FloatingPic -> FloatingPic
encimar p q a b c = union (q a b c) (p a b c)

-- Claramente esto sólo funciona para el ejemplo!
interp :: Output a -> Output (Dibujo a)
interp f a = sem f Interp.rotar Interp.rotar45 Interp.espejar Interp.apilar Interp.juntar Interp.encimar a