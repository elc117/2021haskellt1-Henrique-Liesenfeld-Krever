import Text.Printf


import System.Random (randomRIO)

type Point     = (Int,Int)
type Rect      = (Point,Int,Int)
type Circle    = (Point,Int)


-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Paleta com n valores retirados de uma lista com sequências de R, G e B 

rgbPalette :: Int -> Int -> Int -> Int -> [(Int,Int,Int)]
rgbPalette r1 g1 b1 n = take n $ cycle [(r,g,b)| r <- [0,1..r1], g <- [0,3..g1], b <- [0,7..b1]]



-------------------------------------------------------------------------------
-- Geração de retângulos em suas posições
-------------------------------------------------------------------------------

genRectsInLine :: Int -> [Rect]
genRectsInLine n  = [if y < 9 then(((w+gap)*y, y*(h+gap)), w, h) else (((w+gap)*y, y*(h+gap)-y*(h+gap)), w, h) | y <- [0..fromIntegral (n-1)]] 
  where (w,h) = (50,50)
        gap = 10

genCircles :: (Int,Int) -> Int -> [Circle]
genCircles wh n = [((round(x*7.25)-100,(y*(snd wh) `div` 3)+1),snd wh `div` 3) |y <- reverse [0..fromIntegral 2], x <- reverse[0..fromIntegral (fst(wh))]]

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

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------
main :: IO ()
main = do
  r <- randomRIO(0,255::Int) 
  g <- randomRIO(0,255::Int) 
  b <- randomRIO(0,255::Int) 
  desenha r g b
  
desenha :: Int -> Int -> Int -> IO ()
desenha r g b = do
  writeFile "Henrique_e_suas_incríveis_habilidades_em_haskell.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
        svgfigs = svgElements svgCircle circles (map svgStyle palette)
        palette = rgbPalette r g b ncircles
        circles = genCircles (w,h) ncircles
        ncircles = w*3
        (w,h) = (1500,500) -- width,height da imagem SVG
        -- altura e largura pode ser mudado



{--svgRect rects (map svgStyle palette)),--}