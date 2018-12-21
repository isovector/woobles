
module Main where

import Lib

import Graphics.Rendering.Cairo
import Data.Random
import Data.Random.Source.StdGen
import Data.List


import Debug.Trace

import  Data.Time.Clock.POSIX
import Data.Foldable
import Control.Monad.State

-- this function is something I want to keep a bunch of different versions of
-- each one represents a different project within a given environment
-- and for each sketch the paramters to it represent an exploration
sketch :: [(Double, (Double, Double))] -> App ()
sketch circleData =  do
  bg
  -- let circleData = zip radii $ zip wobbleFs wobbleMs
  trace (show circleData) $ for_ circleData $ \(r, (f, m)) -> wobbleApproxCircle (175,125) r 360 f m

-- I want the option of animation on hand all the time
animation :: [[(Double, (Double, Double))]] -> [App ()]
animation circles = 
  fmap sketch circles
    
writeSketch :: World -> MyState -> String -> App a -> IO()
writeSketch world myState path theSketch = do
  surface <-
    createImageSurface
      FormatARGB32
      (round $ worldWidth world)
      (round $ worldHeight world)
  _ <- renderWith surface $ do
    scale (scaleFactor world) (scaleFactor world)
    runApp world myState theSketch
  surfaceWriteToPNG surface path

leftPad :: Char -> Int -> String -> String
leftPad c n src = (replicate (n - length src) c) ++ src

main :: IO ()
main = do
  let world = World 300 300 1
  let mystate = MyState
  seed <- round . (*1000) <$> getPOSIXTime
  let src = mkStdGen seed

  let frames = 25
  let radii = [1 :: Double, 2, 3, 5, 8, 13, 21, 34, 55, 88, 143, 231]
  
  let (wobbleFs, src') = runState (replicateM (length radii) (runRVar (uniform (0.1:: Double) 10) StdRandom)) src
  let wobbleMs = evalState (replicateM (length radii) (runRVar (uniform (0.1 :: Double) 5) StdRandom)) src'

  let circleData = zip radii $ zip wobbleFs wobbleMs
  let frameRenders = animation $ fmap (\t -> growCircles t circleData)  [0 .. frames]
  for_ (zip [1 .. frames] frameRenders) $ \(f, r) -> do
    let fileName = "./out/" <>  (leftPad '0' 4 $ show f) <> ".png" 
    writeSketch world mystate fileName r
  pure ()

growCircles :: Int -> [(Double, (Double, Double))] -> [(Double, (Double, Double))]
growCircles by circles =
  (flip fmap) circles $ \(r, (f, m)) -> (r+(5*(fromIntegral by)), (f, m))


-- animation idea
-- have wobbly circles grow in size  DONE
-- and have new ones take old place
  -- .... how would you even...
  -- how determine when a new circle needs to be made?
  -- 

-- how can we write less code to explore alternatives


{-
    TODO:
    - Want to use diagrams for compositing
    - Can still make .png's directly through cairo and then lift them into a Diagram
    - figure out how to give the entire image a "papery" feel
    - is there a way to add paper crinkles?
-}

{-
  - Make: recreate pieces you've seen
  - Think: original ideas you have for pieces
  - Observe: the process. what kind of interaction design opportunities can be found here.
-}
