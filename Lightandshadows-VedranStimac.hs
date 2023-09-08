{-
  Autor: Vedran Štimac
  Datum: 08/09/2023
  Zadatak: Jedna pozicija u gridu je izvor svijetla. Na ostatku grida je random
  generirano "kamenje" koje je prepreka svijetlu. Prikazati kako se osvijetli okolina
  uzimajući u obzor to kamenje.
  kamenje=prepreka
  Podsjetnik: Pokrenite program nekoliko puta da vidite razlike.
-}

import System.Random

-- Definiraj tip Grid kao 2D listu znakova
type Grid = [[Char]]

-- Konstante za dimenzije Grid-a
širinaGrida :: Int
širinaGrida = 20

visinaGrida :: Int
visinaGrida = 10

-- Inicijaliziraj prazan Grid ispunjen prazninama
prazanGrid :: Grid
prazanGrid = replicate visinaGrida (replicate širinaGrida ' ')

-- Konstanta za intenzitet svjetla i granicu/rub Grid-a
rub :: Int
rub = 5

-- Funkcija za dodavanje prepreka (kamenja) na slučajne pozicije
dodajPrepreke :: Grid -> Int -> IO Grid
dodajPrepreke grid 0 = return grid
dodajPrepreke grid n = do
  x <- randomRIO (0, širinaGrida - 1) -- Slučajna x-koordinata
  y <- randomRIO (0, visinaGrida - 1) -- Slučajna y-koordinata
  let ažuriraniGrid = postaviPrepreku grid x y
  dodajPrepreke ažuriraniGrid (n - 1) -- Rekurzivno dodaj više prepreka

-- Funkcija za postavljanje prepreke na određenu poziciju u Grid-u
postaviPrepreku :: Grid -> Int -> Int -> Grid
postaviPrepreku grid x y = zamijeniElement grid x y '#'

-- Funkcija za postavljanje svjetla na određenu poziciju u Grid-u
postaviSvjetlo :: Grid -> Int -> Int -> Grid
postaviSvjetlo grid x y = zamijeniElement grid x y '.'

-- Funkcija za širenje svjetla iz izvora
širiSvjetlo :: Grid -> (Int, Int) -> Int -> Grid
širiSvjetlo grid _ 0 = grid
širiSvjetlo grid (x, y) intenzitet
  | izvanGranica x y || jePrepreka grid x y = grid
  | otherwise =
    let ažuriraniGrid = postaviSvjetlo grid x y
    in foldl (\g (dx, dy) -> širiSvjetlo g (x + dx, y + dy) (intenzitet - 1))
             ažuriraniGrid [(1, 0), (-1, 0), (0, 1), (0, -1)] -- Rekurzivno širi svjetlo

-- Funkcija za provjeru je li pozicija izvan granica
izvanGranica :: Int -> Int -> Bool
izvanGranica x y = x < 0 || x >= širinaGrida || y < 0 || y >= visinaGrida

-- Funkcija za provjeru sadrži li pozicija prepreku
jePrepreka :: Grid -> Int -> Int -> Bool
jePrepreka grid x y = grid !! y !! x == '#'

-- Funkcija za zamjenu elementa na određenoj poziciji u Grid-u
zamijeniElement :: Grid -> Int -> Int -> Char -> Grid
zamijeniElement grid x y noviElem =
  take y grid ++
  [take x (grid !! y) ++ [noviElem] ++ drop (x + 1) (grid !! y)] ++
  drop (y + 1) grid

-- Funkcija za osvjetljavanje okoline
osvijetliOkolinu :: Grid -> (Int, Int) -> Int -> Grid
osvijetliOkolinu grid _ 0 = grid
osvijetliOkolinu grid (x, y) intenzitet = 
  foldl (\g (dx, dy) -> 
           if not (izvanGranica (x + dx) (y + dy))
              then postaviSvjetlo g (x + dx) (y + dy)
              else g
        ) grid [(1, 0), (-1, 0), (0, 1), (0, -1)]

-- Glavna funkcija
main :: IO ()
main = do
  gridSPreprekama <- dodajPrepreke prazanGrid 30 -- Dodaj 50 prepreka na prazan Grid
  slučajnaX <- randomRIO (rub, širinaGrida - rub) -- Generiraj slučajnu x-koordinatu izvora svjetla
  slučajnaY <- randomRIO (rub, visinaGrida - rub) -- Generiraj slučajnu y-koordinatu izvora svjetla
  let pozicijaIzvoraSvjetla = (slučajnaX, slučajnaY) -- Kombiniraj x i y koordinate u tuple

  -- Širi svjetlo iz slučajnog izvora s određenim intenzitetom
  let konačniGrid = širiSvjetlo gridSPreprekama pozicijaIzvoraSvjetla rub
  
  -- Ispisuj konačni Grid
  mapM_ putStrLn konačniGrid
