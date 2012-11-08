module GTL.Example.FictitiousPlay.Setup where

import qualified GTL.Game.TwoPlayer.Shapley as S
import qualified GTL.Game.TwoPlayer.RockPaperScissors as R
import qualified GTL.Game.TwoPlayer.BattleOfTheSexes.Asymmetrical as BA
import qualified GTL.Game.TwoPlayer.BattleOfTheSexes.Symmetrical as BS
import qualified GTL.Game.ThreePlayer.Jordan as J

import GTL.Data.Time (Time)
import GTL.Learning.FictitiousPlay (fictitiousPlay)
import GTL.Learning.FictitiousPlay.Plotting (fpPlots)
import GTL.Plotting (figure, subfigures)

import Graphics.Rendering.Plot (Plot)
import Graphics.Rendering.Plot.Gtk (display, PlotHandle)
import Graphics.Rendering.Plot.Render (writeFigure, OutputType( PDF ))

import System.IO (FilePath)
import System.FilePath.Posix ((</>))
import System.Directory (createDirectoryIfMissing)

s  = fictitiousPlay (S.u1, S.u2)       S.perfectInfo  (S.C, S.D)
r  = fictitiousPlay (R.u1, R.u2)       R.perfectInfo  (R.R2, R.R1)
ba = fictitiousPlay (BA.uM, BA.uW)     BA.perfectInfo (BA.Ow, BA.Fm)
bs = fictitiousPlay (BS.uM, BS.uW)     BS.perfectInfo (BS.Ow, BS.Fm)
j  = fictitiousPlay (J.u1, J.u2, J.u3) J.perfectInfo  ((J.T2, J.H3), (J.H1, J.H3), (J.H1, J.H2))

sp  = fpPlots . s
rp  = fpPlots . r
bap = fpPlots . ba
bsp = fpPlots . bs
jp  = fpPlots . j

pdfIt :: FilePath -> [Plot ()] -> IO ()
pdfIt filename ps = writeFigure PDF filename (w, l) $ subfigures ps
    where w = 500
          l = 500 * length ps

pdfAll :: FilePath -> Time -> IO ()
pdfAll dir t = do
  createDirectoryIfMissing True dir
  pdfIt (dir </> "shapley_" ++ show t ++ ".pdf")                      $ sp  t
  pdfIt (dir </> "rockpaperscissors_" ++ show t ++ ".pdf")            $ rp  t
  pdfIt (dir </> "battleofthesexesasymmetrical_" ++ show t ++ ".pdf") $ bap t
  pdfIt (dir </> "battleofthesexessymmetrical_" ++ show t ++ ".pdf")  $ bsp t
  pdfIt (dir </> "jordan_" ++ show t ++ ".pdf")                       $ jp  t

displayThem :: [Plot ()] -> IO [PlotHandle]
displayThem = mapM (display . figure)

displayIt :: [Plot ()] -> IO PlotHandle
displayIt = display . subfigures