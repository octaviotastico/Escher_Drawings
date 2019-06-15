module Main where
import System.Directory
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.UI.GLUT.Begin
import Interp
import Dibujo
import Pred
import qualified Basico.Escher as E
import qualified Basico.EscherVectorizado as EV

data Conf a = Conf {
  basic :: Output a,
  fig  :: Dibujo a,
  width :: Float,
  height :: Float
}

ej1 x y = Conf {
  basic = EV.interpBas,
  fig = EV.dibujo,
  width = x,
  height = y
}

ej2 x y f = Conf {
  basic = E.interpBas f,
  fig = E.dibujo,
  width = x,
  height = y
}


-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: IO (Conf a) -> IO ()
initial cf = cf >>= \cfg ->
                let x  = width cfg
                    y  = height cfg
                    img = interp (basic cfg) (fig cfg) (0,0) (x,0) (0,y)
                in display win white img
    where
        withGrid p = pictures [p, color grey $ grid 10 (0,0) 100 10]
        grey = makeColorI 120 120 120 120

win = InWindow "Escher" (500, 500) (0, 0)

format_files :: [FilePath] -> [FilePath]
format_files files = map (\x -> comp init 4 x) (
                        filter (\x -> x /= "." && x /= "..") files)

list_strings :: [String] -> IO()
list_strings [x] =  do
    putStrLn x
list_strings (x:xs) = do 
    putStrLn x
    list_strings xs

opciones :: [String]
opciones = ["1 - Escher vectorizado", "2 - Escher con imagenes", "3 - Escher con figuras predefinidas"]

figuras :: [String]
figuras = ["1 - trian1", "2 - trian2", "3 - trianD", "4 - rectan", "5 - fShape", "6 - curvita"]

mapfig :: String -> FloatingPic
mapfig n = case (read n :: Int) of
    1 -> trian1
    2 -> trian2
    3 -> trianD
    4 -> rectan
    5 -> fShape
    6 -> curvita
    _ -> Interp.blank

main :: IO ()
main = do
    putStrLn "Elija entre estas 3 opciones:"
    list_strings opciones
    option <- getLine
    if option == "1" then do
        initial $ return (ej1 100 100)

    else if option == "2" then do
        putStrLn "Elija una imagen de las siguientes:"
        files <- getDirectoryContents "./img"
        list_strings (format_files files)
        imgName <- getLine
        image@(Bitmap bmpData) <- loadBMP ("./img/" ++ imgName ++ ".bmp") 
        let (width, height) = bitmapSize bmpData 
        let w = fromIntegral width
        let h = fromIntegral height
        initial $ return (ej2 w h (transf (\p _ -> p) image (w, h)))
    
    else if option == "3" then do
        putStrLn "Elija una figura de las siguientes:"
        list_strings figuras
        fig <- getLine
        initial $ return (ej2 100 100 (mapfig fig))
    else do
        putStrLn "Opcion no disponible"