> {-# OPTIONS -XFlexibleInstances #-}
> {-# OPTIONS -XTypeSynonymInstances #-}

> module Euterpea (
>   module Euterpea.Music.Note.Music,
>   module Euterpea.IO.Audio,
>   module Euterpea.IO.MIDI,
>   module Control.Arrow,
>   -- This next line is from Codec.Midi
>   exportFile, importFile
>   ) where
>
> import Euterpea.Music.Note.Music 
> import Euterpea.IO.Audio
> import Euterpea.IO.MIDI
> import Control.Arrow
> import Codec.Midi(exportFile, importFile)
> import Control.Arrow.Operations

