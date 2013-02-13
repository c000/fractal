import Control.Parallel (pseq)
import Control.Parallel.Strategies
import Codec.Picture
import Data.Complex
import qualified Data.Vector.Storable as V

rangeArray :: [Double]
rangeArray = map (/40950) [0..4095]

complexRange :: [Complex Double]
complexRange = [ (x - 1.8) :+ (y - 0.09) | y <- rangeArray, x <- rangeArray]

execBurn :: Complex Double -> Complex Double -> Complex Double
execBurn c (x:+y) = (abs x :+ abs y) * (abs x :+ abs y) + c

lengthBurn :: Complex Double -> Pixel8
lengthBurn c = fromIntegral . length $ (take 255 . takeWhile comp) bList
  where
    comp x = magnitude x < 10
    bList = iterate (execBurn c) 0

linearImage :: [Pixel8]
linearImage = map lengthBurn complexRange `using` parListChunk 1024 rdeepseq

myImage :: Image Pixel8
myImage = Image 4096 4096 (V.fromList linearImage)

main = do
	savePngImage "BurningShip.png" (ImageY8 myImage)
