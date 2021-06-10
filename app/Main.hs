import Text.Printf


import System.Random (randomRIO)

type Point     = (Int,Int)
type Rect      = (Point,Int,Int)
type Circle    = (Point,Int)


-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Paleta (R, G, B) só com tons de verde "hard-coded" 
-- (pode ser melhorado substituindo os valores literais por parâmetros)
-- Além disso, o que acontecerá se n for muito grande ou negativo?
greenPalette :: Int -> [(Int,Int,Int)]
greenPalette n = [(0, 80+i*10, 0) | i <- [0..n] ]

-- Paleta com n valores retirados de uma lista com sequências de R, G e B 
-- O '$' é uma facilidade sintática que substitui parênteses
-- O cycle é uma função bacana -- procure saber mais sobre ela :-)
rgbPalette :: Int -> Int -> Int -> Int -> [(Int,Int,Int)]
rgbPalette r1 g1 b1 n = take n [(r,g,b)| r <- [0,1..r1], g <- [0,3..g1], b <- [0,7..b1]]



-------------------------------------------------------------------------------
-- Geração de retângulos em suas posições
-------------------------------------------------------------------------------

genRectsInLine :: Int -> [Rect]
genRectsInLine n  = [if y < 9 then(((w+gap)*y, y*(h+gap)), w, h) else (((w+gap)*y, y*(h+gap)-y*(h+gap)), w, h) | y <- [0..fromIntegral (n-1)]] 
  where (w,h) = (50,50)
        gap = 10

genCircles :: Int -> [Circle]
genCircles n = [((round(x*7.25)-100,y*170),170) |y <- reverse [0..fromIntegral 2], x <- reverse[0..fromIntegral 1500]]
 where
   (w,h)=(1500,500)

-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%d' y='%d' width='%d' height='%d' style='%s' />\n" x y w h style

svgCircle :: Circle -> String -> String 
svgCircle ((x,y),r) style = 
  printf "<circle cx='%d' cy='%d' r='%d' fill='%s' />\n" x y r style

-- String inicial do SVG
svgBegin :: Int -> Int -> String
svgBegin w h = printf "<svg width='%d' height='%d' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "rgb(%d, %d ,%d, 1)" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

--aleatorio :: Int
--aleatorio = randomRIO (0,255::Int)
-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------
main :: IO ()
main = do
  r <- randomRIO(0,255::Int) 
  g <- randomRIO(0,255::Int) 
  b <- randomRIO(0,255::Int) 
  lala r g b
  
lala :: Int -> Int -> Int -> IO ()
lala r g b = do
  writeFile "Henrique_e_suas_incríveis_habilidades_em_haskell.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
        svgfigs = svgElements svgCircle circles (map svgStyle palette)
        palette = rgbPalette r g b ncircles
        circles = genCircles ncircles
        ncircles = 4500
        (w,h) = (1500,500) -- width,height da imagem SVG



{--svgRect rects (map svgStyle palette)),--}