module Euterpea.IO.Audio
  ( module Euterpea.IO.Audio.BasicSigFuns,
    module Euterpea.IO.Audio.Basics,
    module Euterpea.IO.Audio.Types,
    module Euterpea.IO.Audio.IO,
    module Euterpea.IO.Audio.Render,
    writeWav,
    writeWavNorm
  ) where

import Euterpea.IO.Audio.BasicSigFuns
import Euterpea.IO.Audio.Basics
import Euterpea.IO.Audio.Types
import Euterpea.IO.Audio.IO
import Euterpea.IO.Audio.Render

writeWav fname iMap m = 
    let (d,s) = renderSF m iMap
    in  outFile fname d s

writeWavNorm fname iMap m = 
    let (d,s) = renderSF m iMap
    in  outFileNorm fname d s

