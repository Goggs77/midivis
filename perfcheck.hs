import Control.Exception (evaluate)
import System.CPUTime
import Graphics.Gloss.Data.Color (makeColor)
import Midivis.Render.Text (utf8PictureColored)
import Midivis.Resources.Font (fontAtlas)

main :: IO ()
main = do
    t0 <- getCPUTime
    let p = utf8PictureColored (makeColor 1 1 1 0.95) "Dastgah-e Abuata - 12 TET"
    let n = length (show p)
    t1 <- getCPUTime
    putStrLn $ "pic spine force ok, nodes~" ++ show n ++ " in " ++ show (fromIntegral (t1 - t0) / 1e12 :: Double) ++ " s"
    putStrLn $ "atlas pages: " ++ show (length fontAtlas)
