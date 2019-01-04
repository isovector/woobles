{-# LANGUAGE ApplicativeDo #-}

module Main where

import Lib
import Exposition

import Data.Random
import Data.Random.Source.StdGen
import Data.Coerce

import Data.Time.Clock.POSIX
import Control.Monad.State
import Control.Applicative

import Linear.V2
import Diagrams.Backend.SVG
import Diagrams.Size
import Diagrams.Prelude
import Data.Colour.Palette.Types
import Web.Suavemente

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let rsrc = mkStdGen seed
  colors <- myColors 201
  suavemente $ do
    ringWidth <- realSlider "Ring Width" 0.05 0.5 0.01 0.05
    ringTotal <- slider "Radius" 50 100 70
    freqLow <- slider "Low Freq" 1 20 4
    freqHigh <- slider "High Freq" 1 20 8
    phaseLow <- realSlider "Low Phase" 0 pi 0.01 (pi / 4)
    phaseHigh <- realSlider "High Phase" 0 pi 0.01 pi
    pure $ art rsrc colors ringWidth ringTotal (freqLow, freqHigh) (phaseLow, phaseHigh)

  --art
  -- renderCairo "out/figures/magnitude.png" (dims $ V2 300 300) magnitude

  -- renderCairo "out/figures/frequency.png" (dims $ V2 300 300) frequency
  --renderCairo "out/test.png" (dims $ V2 1600 900) (diagram # bg black # curatedRegion)
  --pure ()

art :: StdGen
    -> [Kolor]
    -> Double  -- width
    -> Double  -- radius
    -> (Double, Double) -- freq band
    -> (Double, Double) -- phase band
    -> Diagram B
art rsrc colors width rad (flow, fhigh) (plow, phigh) =
  let radii  = [0.1 :: Double, (0.1 + width) .. rad]
      numWoobles = length radii
      (freqs, rsrc')  = runState (replicateM numWoobles $ runRVar (uniform flow fhigh) StdRandom) rsrc
      phases = evalState (replicateM numWoobles $ runRVar (uniform plow phigh) StdRandom) rsrc'

      woobles = getZipList $
              (\c r f p -> wooble' r (Wobble f (r/75) p) c)
                <$> coerce colors
                <*> coerce radii
                <*> coerce freqs
                <*> coerce phases
      diagram = foldr (\d acc -> center d `atop` acc) mempty woobles
      curatedRegion = rectEnvelope (p2 (-48, -40)) (r2 (40, 40))
    in diagram # bg black # curatedRegion


{-
  - Make: recreate pieces you've seen
  - Think: original ideas you have for pieces
  - Observe: the process. what kind of interaction design opportunities can be found here.
-}
