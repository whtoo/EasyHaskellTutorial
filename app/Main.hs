module Main where
import Graphics.Gloss

main :: IO ()
main = display window backgroud drawing
    where
        window = InWindow "Nice Window" (200,200) (0,0)
        backgroud = white
        drawing = Circle 80

