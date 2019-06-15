module Basico.Escher where
import Dibujo
import Interp
import Prelude

-- supongamos que eligen 
type Escher = Bool 

fish :: Dibujo Escher
fish = Bas True 

-- el dibujo u
dibujo_u :: Dibujo Escher -> Dibujo Escher
dibujo_u p = (encimar4 fish2)
    where
        fish2 = Espejar (Rot45 p)

-- el dibujo t
dibujo_t:: Dibujo Escher ->  Dibujo Escher
dibujo_t p = Encimar p (Encimar fish2 fish3)
    where
        fish2 = Espejar (Rot45 p)
        fish3 = r270 fish2 

-- esquina con nivel de detalle en base a la figura p
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 0 _ = Bas False
esquina n p = cuarteto (esquina (n - 1) p) (lado (n - 1) p) (Rotar (lado (n - 1) p)) (dibujo_u p)

-- lado con nivel de detalle
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 0 _ = Bas False
lado n p = cuarteto (lado (n - 1) p) (lado (n - 1) p) (Rotar (dibujo_t p)) (dibujo_t p)

-- por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x = Apilar 1 2 (Juntar 1 2 p (Juntar 1 1 q r))
    (Apilar 1 1 (Juntar 1 2 s (Juntar 1 1 t u)) (Juntar 1 2 v (Juntar 1 1 w x)))

-- el dibujo de Escher:
escher :: Int -> Dibujo Escher -> Dibujo Escher
escher n p = noneto (esquina n p) (lado n p) (r270 (esquina n p)) (Rotar (lado n p))
    (dibujo_u p) (r270 (lado n p)) (Rotar (esquina n p)) (r180 (lado n p)) (r180 (esquina n p))

dibujo :: Dibujo Escher
dibujo = escher 3 fish
-- dibujo = escher 3 fish

interpBas :: FloatingPic -> Output Escher
interpBas f True = f
interpBas _ False = blank