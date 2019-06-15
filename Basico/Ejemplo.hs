module Basico.Ejemplo where
import Dibujo
import Interp
import Prelude

type Basica = Bool 
ejemplo1 :: Dibujo Basica
ejemplo1 = Juntar 10 1 (Bas False) (Bas True)

ejemplo2 :: Dibujo Basica
ejemplo2 = comp Dibujo.espejar 100 (Bas True)

interpBas :: Output Basica
interpBas True = fShape
interpBas False = trian1
