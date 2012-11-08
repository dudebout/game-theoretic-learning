{-# LANGUAGE DeriveDataTypeable #-}

import GTL.Example.FictitiousPlay.Setup
import GTL.Data.Time
import System.Console.CmdArgs

data PDFArgs = PDFArgs { dir     :: FilePath
                       , endTime :: Time } deriving (Show, Data, Typeable)

pdfArgs = PDFArgs { dir = def &= args &= typ "FILES/DIRS", endTime = 1000 }

main = do
  as <- cmdArgs pdfArgs
  let d = dir as
      t = endTime as
  if null $ dir as
     then error "A directory name need to be provided"
     else pdfAll d t
